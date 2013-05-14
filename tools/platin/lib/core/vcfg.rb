#
# platin tool set
#
# virtual control-flow model (virtual callgraph + cfg)
#
require 'core/pml'

module PML

#
# Flat Control Flow Model
# This is a simpler model without loop/region subgraphs
#

# Necessary flow information to build control-flow model
class FlowInformation
  def is_infeasible(block, context = nil)
    false
  end
  def get_calltargets(callsite, context = nil)
    if callsite.unresolved_call?
      raise Exception.new("Unresolved call")
    else
      callsite.callees
    end
  end
end


# Flat Control-Flow Model, consists of a set of virtual CFGs
class ControlFlowModel
  attr_reader :ctx_manager, :vcfgs
  def initialize(function_list, entry, ctx_manager, opts)
    @function_list, @entry, @ctx_manager, @opts = function_list, entry, ctx_manager, opts
    @vcfgs = { @entry => VCFG.new(@entry, opts) }
  end
  #
  # return the entry node for the given function
  #
  def get_entry_node(f)
    vcfg = @vcfgs[f]
    unless vcfg
      vcfg = @vcfgs[f] = VCFG.new(f, @opts)
    end
    vcfg.entry
  end
  #
  # return VCFGs of functions possible called at this location
  #
  def each_callee(loc)
    # puts "CALLSITE: #{loc.node.callsite.inspect}"
    if loc.node.callsite.unresolved_call?
      raise Exception.new("Indirect calls not yet supported: #{loc.node.callsite}")
    end
    loc.node.callsite.callees.each { |flabel|
      yield get_entry_node(@function_list.by_label(flabel)).vcfg
    }
  end
end

#
# Virtual CFG, consists of Entry, Exit, Call and BlockSlice nodes
class VCFG
  include QNameObject

  attr_reader :function, :nodes, :entry, :exit

  def initialize(function, opts)
    opts[:delay_slots] ||= 0
    opts[:call_delay_slots] ||= opts[:delay_slots]
    opts[:return_delay_slots] ||= opts[:delay_slots]
    @function = function
    @qname = "V#{@function.qname}"
    @nodes = []
    @entry = EntryNode.new(self)
    @exit = ExitNode.new(self)

    # create nodes for all basic blocks and call / return sites
    first_nodes, last_nodes = build_basic_block_nodes(function, opts)

    # link nodes
    @entry.add_successor(first_nodes[function.blocks.first])
    function.blocks.each do |block|
      block.successors.each do |succ|
        npred, nsucc = last_nodes[block], first_nodes[succ]
        assert("No node matching block #{succ}") { nsucc }
        # if successors is in a different loop, we need to insert loop exit nodes
        if block.loopnest > 0 && succ.loops.first != block.loops.first
          loopix = 0
          # exit loops until successor level is equal and matching, or smaller
          while block.loopnest - loopix >= succ.loopnest && block.loops[loopix] != succ.loops.first
            npred = insert_loop_node(npred, block.loops[loopix], :exit)
            loopix += 1
          end
        end
        # if succ is a loop header, we insert loop enter/cont nodes
        if succ.loopheader?
          if block.loops.include?(succ) # continue
            npred = insert_loop_node(npred, succ, :cont)
          else
            npred = insert_loop_node(npred, succ, :enter)
          end
        end
        npred.add_successor(nsucc)
      end
      # If no is-return information is available for instructions
      if block.successors.empty?
        last_nodes[block].add_successor(@exit) unless last_nodes[block].successors.any? { |n| n.exit? }
      end
    end
  end

private

  def build_basic_block_nodes(function, opts)
    first_nodes, last_nodes = {}, {}
    function.blocks.each do |block|
      if(block.instructions.size == 0)
        first_nodes[block] = last_nodes[block] = BlockSliceNode.new(self, block, 0, -1)
        next
      end
      split_node, index = nil, 0
      current_node = BlockSliceNode.new(self, block, 0)
      first_nodes[block] = current_node
      while(index < block.instructions.size)
        current_instruction = block.instructions[index]
        if split_node
          current_node = BlockSliceNode.new(self, block, index)
          split_node.add_successor(current_node)
          split_node = nil
        end
        if current_instruction.calls?
          current_node.last_index = index + opts[:call_delay_slots]
          index = current_node.last_index + 1
          call_node = CallNode.new(self, current_instruction)
          current_node.add_successor(call_node)
          split_node = call_node
        elsif current_instruction.returns?
          current_node.last_index = index + opts[:return_delay_slots]
          index = current_node.last_index + 1
          current_node.add_successor(@exit)
          split_node = current_node
        else
          index += 1
        end
      end
      current_node.last_index = block.instructions.size - 1
      last_nodes[block] = current_node
    end
    return [first_nodes, last_nodes]
  end

  def insert_loop_node(pred, loop, action)
    loop_node = LoopStateNode.new(self, loop, action)
    pred.add_successor(loop_node)
    loop_node
  end
end

# VCFG Nodes
# Identified by ID unique to the VCFG
class CfgNode
  #
  #  For efficiency reasons, we do not use qnames here
  #  That is, objects are compared by pointer equality
  #
  #  include QNameObject

  # intra-procedural control-flow successors
  attr_reader :vcfg, :nid, :successors, :predecessors
  def initialize(vcfg)
    @vcfg, @nid, @successors, @predecessors = vcfg, vcfg.nodes.size, [], []
#    @qname = "#{@vcfg.qname}/#{@nid}"
    @vcfg.nodes.push(self)
  end

  def add_successor(node)
    @successors.push(node)
    node.predecessors.push(self)
  end
  def set_index(ix)
    @nid = ix
  end
  def callnode? ; false ; end
  def instructions ; [] ; end
  def entry? ; false ; end
  def exit? ; false ; end
end

class EntryNode < CfgNode
  def initialize(vcfg) ; super(vcfg); end
  def entry? ; true ; end
  def to_s
    "#<EntryNode #{vcfg.function}>"
  end
end

class ExitNode < CfgNode
  def initialize(vcfg) ; super(vcfg) ; end
  def exit? ; true ; end
  def to_s
    "#<ExitNode #{vcfg.function}>"
  end
end

class BlockSliceNode < CfgNode
  attr_reader :block, :first_index
  attr_accessor :last_index
  def initialize(vcfg, block, first_ins, last_ins = -1)
    super(vcfg)
    @block, @first_index, @last_index = block, first_ins, last_ins
  end
  def instructions
    @block.instructions[@first..@last]
  end
  def to_s
    "#<BlockSliceNode #{block} #{first_index} #{last_index}>"
  end
end

class LoopStateNode < CfgNode
  attr_reader :loop, :action
  # action is one out of :enter, :exit, :cont
  def initialize(vcfg, loop, action)
    super(vcfg)
    @loop, @action = loop, action
  end
  def to_s
    "#<LoopStateNode #{loop} #{action}>"
  end
end
class CallNode < CfgNode
  attr_reader :callsite
  def initialize(vcfg, callsite)
    super(vcfg)
    @callsite = callsite
  end
  def callnode? ; true; end
  def to_s
    "#<CallNode #{callsite}>"
  end
end

#
# Simple abstract interpretation on flat, inter-procedural control-flow model
# Following state-of-the-art AI frameworks, we separate abstract values and
# locations (which are an abstract of program counter and history) for efficiency
# reasons
#
class Interpreter
  attr_reader :steps
  def initialize(semantics, cfmodel)
    @semantics, @cfmodel, @ctx_manager = semantics, cfmodel, cfmodel.ctx_manager
    @steps = 0
  end
  def interpret(initial_node, start_value)
    @in, @out, @contexts = {}, {}, {}
    @callcontexts, @returncontexts, @loopcontexts = {}, {}, {}
    initial_loc = Location.new(initial_node, @ctx_manager.initial)
    @in[initial_loc] = start_value
    @queue = [initial_loc]
    while ! @queue.empty?
      @steps += 1
      loc = @queue.pop
      inval = @in[loc]
      outval  = @semantics.transfer_value(loc.node, inval)
      compute_successors(loc, outval)
    end
  end
  def compute_successors(loc, outval)
    @contexts[loc.node] ||= Set.new
    @contexts[loc.node].add(loc.context)

    case loc.node
    when EntryNode
      loc.node.successors.each { |s| process_successor(s, loc.context, outval) }
    when BlockSliceNode
      next_context = @ctx_manager.blockslice(loc.context, loc.node)
      loc.node.successors.each { |s| process_successor(s, next_context, outval) }
    when CallNode
      # add call to context
      callee_context = @ctx_manager.push_call(loc.context, loc.node)

      # remember the call-context part of the current context as candidate for returns
      call_return_context = record_call_return(loc.context, callee_context)

      # yield locations at callee's entries
      @cfmodel.each_callee(loc) { |f|

        process_successor(f.entry, callee_context, outval)

        # back-propagate results from matching return contexts (in case this function was already analyzed)
        @returncontexts[loc.node][call_return_context].each { |callee_context_at_return|
          retval = @in[Location.new(f.exit,callee_context_at_return)]
          if ! retval
            raise Exception.new("No return value for #{callee_context_at_return} matching #{call_return_context}")
          end
          return_context, callnode = @ctx_manager.pop_call(callee_context_at_return)
          refined_context = return_context.with_caller_context(loc.context)
          loc.node.successors.each { |n| process_successor(n, refined_context, retval) }
        }
      }
    when ExitNode
      # Contexts [example] [example]
      # loc.context    ... context at exit                                      [main,f1,g2]+x  [f1,g2,h3]+x    [f1,g2,h3]+x
      # return_context ... context at exit with topmost callstack entry popped  [main,f1]+x     [f1,g2]+x       [f1,g2]+x
      # call_context   ... context at callsite to function                      [main,f1]+y     [main,f1,g2]+y  [main,f2,g2]+y
      # refined_context .. return_context with refined call context             [main,f1]+x     [main,f1,g2]+x  NONE

      # get return context
      return_context, callnode = @ctx_manager.pop_call(loc.context)
      # handle program exit
      return if ! callnode

      # get call/return context
      call_return_context = return_context.call_context_slice
      @returncontexts[callnode][call_return_context].add(loc.context)

      # refine call contexts and yield call successors
      @callcontexts[callnode][call_return_context].each { |call_context|
        refined_context = return_context.with_caller_context(call_context)
        callnode.successors.each { |s| process_successor(s, refined_context, outval) }
      }
    when LoopStateNode
      case loc.node.action
      when :exit
        return_context, loopnode = @ctx_manager.exit_loop(loc.context, loc.node.loop)
        if loopnode
          @loopcontexts[loopnode][return_context.call_context_slice].each { |enter_context|
            refined_context = return_context.with_caller_context(enter_context)
            loc.node.successors.each { |s| process_successor(s, refined_context, outval) }
          }
        else
          loc.node.successors.each { |s| process_successor(s, return_context, outval) }
        end
      when :enter
        next_context = @ctx_manager.enter_loop(loc.context, loc.node)
        record_loop(loc.node.loop, loc.context, next_context)
        loc.node.successors.each { |s| process_successor(s, next_context, outval) }
      when :cont
        next_context = @ctx_manager.continue_loop(loc.context, loc.node.loop);
        loc.node.successors.each { |s| process_successor(s, next_context, outval) }
      else raise Exception.new("Bad loop state action")
      end
    else
      raise Exception.new("Bad VCFG node type #{loc.node.class}")
    end
  end

  def process_successor(node, context, val)
    loc = Location.new(node, context)
    if change = @semantics.merge(@in[loc],val)
      @in[loc] = change[1]
      @queue.push(loc)
    end
  end
  def stats(io=$stdout)
    io.puts "Nodes: #{@cfmodel.vcfgs.values.map { |cfg| cfg.nodes.length }.inject(0,:+)}"
    io.puts "Locations: #{@contexts.values.map { |ctx| ctx.size }.inject(0,:+)}"
    io.puts "Steps: #{steps}"
  end
  def dump(io=$stdout)
    @contexts.each do |node, ctxs|
      io.puts " NODE: #{node} #{node.qname}#{" #{node.block}" if node.respond_to?(:block)}"
      ctxs.each do |ctx|
        loc = Location.new(node, ctx)
        io.puts "  CONTEXT: #{ctx}"
        io.puts "  VALUE: #{@in[loc]}"
      end
    end
  end
private
  def record_loop(loop, entry_context, loop_start_context)
    return_context, loopnode = @ctx_manager.exit_loop(loop_start_context, loop)
    return unless loopnode
    call_return_context = return_context.call_context_slice
    dict = (@loopcontexts[loopnode] ||= {})
    dict[call_return_context] ||= Set.new
    dict[call_return_context].add(entry_context)
  end
  def record_call_return(callsite_context, callee_context)
    return_context, callnode = @ctx_manager.pop_call(callee_context)
    call_return_context = return_context.call_context_slice
    dict = (@callcontexts[callnode] ||= {})
    dict[call_return_context] ||= Set.new
    dict[call_return_context].add(callsite_context)
    rdict = (@returncontexts[callnode] ||= {})
    rdict[call_return_context] ||= Set.new
    call_return_context
  end
end

# Top / Bottom
TOP=:top
BOTTOM=:bottom

# Locations are pairs of VCFG nodes and context
class Location
  attr_reader :node, :context
  def initialize(node, context)
    @node, @context = node, context
  end
  def to_s
    "#<Location #{@node} #{@context}>"
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(Location)
    @node == other.node && @context == other.context
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash=@node.hash ^ @context.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end


# Stacks with bounded memory
class BoundedStack
  # fly-weight, to get cheaper comparisons (profile hotspot)
  @@repository = {}
  attr_reader :stack
  def BoundedStack.create(stack)
    bs = @@repository[stack]
    if ! bs
      bs = @@repository[stack] = BoundedStack.new(stack)
    end
    bs
  end
  def initialize(elems)
    @stack = elems
    @cache = {}
  end
  def BoundedStack.initial(length)
    BoundedStack.create([])
  end
  def push(elem, maxlength)
    return if maxlength == 0
    return BoundedStack.create([elem]) if maxlength==1
    if stack.length < maxlength
      BoundedStack.create(stack + [elem])
    else
      BoundedStack.create(stack[1..-1] + [elem])
    end
  end
  def pop
    newstack = stack.dup
    newstack.pop
    BoundedStack.create(newstack)
  end
  def top
    @stack[-1]
  end
  def map_top
    newstack = stack.dup
    newelem = yield newstack.pop
    newstack.push(newelem)
    BoundedStack.create(newstack)
  end
  def to_s
    return "[]" if @stack.empty?
    @stack.map { |n| n.to_s }.join("-")
  end
  # unique objects - can use pointer equality (== defined as equals?) per default
  def hash
    return @hash if @hash
    @hash = stack.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

# Graph-based management of callstrings (unique objects)
#
# Example (callstring length 2)
# ----------------------------------------
# start:        main ~> top -> top
# main/2 -> f:  f    ~> main/2 -> top
# f/3 -> g:     g    ~> f/3 -> main/2
# f/5 -> g:     g    ~> f/5 -> main/2
# g/2 -> h:     h    ~> g/2 -> f/5
# main/3 -> f:  f    ~> main/3 -> top
# f/5 -> g:     g    ~> f/5 -> main/3
# g/2 -> h:     h    ~> g/2 -> f/5
# main/4 -> g:  g    ~> main/4 -> top
# ----------------------------------------
# Storage Graph               id poplinks
#
# main: top    --> top        1  -
# f:    main/2 --> top        2  1
#       main/3 --> top        3  1
# g:    f/3    --> main/2     4  2
#       f/5    --> main/2     5  2
#              +-> main/3     6  3
#       main/4 --> top        7  1
# h:    g/2    --> f/5        8  5,6
# ---------------------------------------
# not implemented; postponed

# Loop Context for one loop
# Given peel and unroll factors
# loop contexts represented as
#   [loopheader,init,offset]
# correspond to the loop iteration
#   init<peel:  init
#   init==peel: peel + unroll*k + offset
class LoopContext
  attr_reader :loop,:init,:offset
  def initialize(loop,init=0,offset=0)
    if ! loop
      raise Exception.new("Bad loop")
    end
    @loop = loop
    @init = init
    @offset = offset
  end
  def dup
    LoopContext.new(@loop,@init,@offset)
  end
  def with_increment(peel,unroll)
    if @init >= peel
      LoopContext.new(@loop, @init,(@offset + 1) % unroll)
    else
      LoopContext.new(@loop, @init+1,@offset)
    end
  end
  def predecessor_contexts(peel, unroll)
    if @init >= @peel
      if @offset == 0
        [ LoopContext.new(@loop,peel-1,0), LoopContext.new(@loop, @init, unroll - 1) ]
      else
        [ LoopContext.new(@loop,@init,@offset-1) ]
      end
    elsif @init > 0
      [ LoopContext.new(@loop, @init - 1, 0) ]
    else
      []
    end
  end
  def to_s
    "#<LoopContext #{loop} #{init} #{offset}>"
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(LoopContext)
    @loop == other.loop && @init == other.init && @offset == other.offset
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash = @loops.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

# Context
class Context
  attr_reader :callstring, :loopcontext
  def initialize(cs, lc)
    @callstring, @loopcontext = cs, lc
  end
  def Context.initial(calldepth, loopdepth)
    Context.new(BoundedStack.initial(calldepth), BoundedStack.initial(loopdepth))
  end
  def with_callstring
    callstring_new = yield @callstring
    Context.new(callstring_new, @loopcontext)
  end
  def with_loopcontext
    loopcontext_new = yield @loopcontext
    Context.new(@callstring, loopcontext_new)
  end
  def call_context_slice
    # no trace context atm
    self
  end
  def with_caller_context(caller_context)
    # no trace context atm
    caller_context
  end
  def to_s
    "Ctx<#{@callstring},#{@loopcontext}>"
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(Context)
    @callstring == other.callstring && @loopcontext == other.loopcontext
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash= [@callstring,@loopcontext].hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

# fairly generic context manager
class ContextManager
  def initialize(calldepth = 1, loopdepth = 0, looppeel = 1, loopunroll = 1)
    raise Exception.new("ContextManager: calldepth>=1 is required") unless calldepth >= 1
    @calldepth, @loopdepth, @looppeel, @loopunroll = calldepth, loopdepth, looppeel, loopunroll
  end
  def initial
    Context.initial(@calldepth, @loopdepth)
  end
  # do not record instruction history context for now
  def blockslice(ctx, node)
    ctx
  end
  def push_call(ctx, callnode)
    ctx.with_callstring { |cs| cs.push(callnode, @calldepth) }
  end
  def pop_call(ctx)
    callnode = ctx.callstring.top
    if callnode
      [ ctx.with_callstring { |cs| cs.pop }, callnode ]
    else
      nil
    end
  end
  def enter_loop(ctx, loop)
    return ctx unless @loopdepth > 0
    ctx.with_loopcontext { |lc|
      lc.push(LoopContext.new(loop), @loopdepth)
    }
  end
  def exit_loop(ctx, loop)
    return ctx unless @loopdepth > 0
    loopnode = ctx.loopcontext.top.loop
    [ ctx.with_loopcontext { |lc| lc.pop }, loopnode ]
  end
  def continue_loop(ctx,loop)
    return ctx unless @loopdepth > 0
    ctx.with_loopcontext { |lc|
      lc.map_top { |lc| lc.with_increment(@looppeel, @loopunroll) }
    }
  end
end

# reachability semantics (trivial)
class ReachabilitySemantics
  def transfer_value(node, inval) ; inval ; end
  def merge(oldval, newval)
    if oldval
      false # no change
    else
      [true, newval]
    end
  end
end

end # module PML
