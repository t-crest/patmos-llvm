#
# platin tool set
#
# virtual control-flow model (virtual callgraph + cfg)
#
require 'core/pml'
require 'core/context'
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
    @locations, @scope_entries, @scope_exits = {}, {}, {}
    ip = Interpreter.new(ReachabilitySemantics.new, self)
    ip.interpret(get_entry_node(entry), true)
    ip.stats($stdout)
    ip.inputs_by_node.each do |node,ctxs|
      ctxs.keys.each do |ctx|
        node.add_context(ctx)
      end
      puts node
      node.context_tree.dump($stdout,1)
    end
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
  # get VCFG by label of the represented function
  #
  def by_label(label)
    get_entry_node(@function_list.by_label(label)).vcfg
  end


  #
  # get location object (unique)
  #
  def location(node, ctx)
    (@locations[node]||={})[ctx]||=Location.new(self, node,ctx)
  end

  #
  # record scope entry (lazy scope-graph construction)
  #
  # - scope_node is the VCFG node which represents the scope
  # - scope_context_before is the scope-related part of the context before entering the scope
  # - scope_context_entry is the scope-related part of the context after entering the scope
  #
  # Example for functions:
  #   Callnode<f:4> , Context<Call(main:3);Loop(main:1/1);Predecessor(f:3)>: call g
  #     scope_node           := g.entry
  #     scope_context_entry  := Context<Call(f:4);Loop(main:1/1);\top>
  #     scope_context_before := Context<Call(main:3);Loop(main:1/1);\top>
  # Example for loops:
  #   LoopEnter<f:4>, Context<Call(main:3);Loop(main:1/1);Predecessor(f:3)>:
  #     scope_node:          := LoopEnter<f:4>
  #     scope_context_before := Context<Call(main:3);Loop(main:1/1);\top>
  #     scope_context_entry  := Context<Call(main:3);Loop(f:4/1);\top>
  #
  def record_scope_entry(scope_node, scope_context_entry, scope_context_before)
    dict = (@scope_entries[scope_node] ||= {})
    (dict[scope_context_entry] ||= Set.new).add(scope_context_before)
    #puts "Recording scope entry:"
    #puts " scope node: #{scope_node}"
    #puts " scope context entry: #{scope_context_entry}"
    #puts " scope context before:#{scope_context_before}"
  end
  def matching_scope_entries(scope_node, scope_context_entry)
    #puts "Match scope entry:"
    #puts " scope node: #{scope_node}"
    #puts " scope context entry: #{scope_context_entry}"
    #puts " matching scope contexts before: #{@scope_entries[scope_node][scope_context_entry].to_a.join(", ")}"
    @scope_entries[scope_node][scope_context_entry]
  end
  #
  # record known scope exits (lazy scope-graph construction)
  #
  def record_scope_exit(scope_node, scope_context_entry, exit_location)
    dict = (@scope_exits[scope_node] ||= {})
    dict[scope_context_entry] ||= Set.new
    dict[scope_context_entry].add(exit_location)
    #puts "Recording scope exit:"
    #puts " scope node: #{scope_node}"
    #puts " scope context entry: #{scope_context_entry}"
    #puts " exit location: #{exit_location}"
  end
  def matching_scope_exits(scope_node, scope_context_entry)
    #puts "Match scope exits:"
    #puts " scope node: #{scope_node}"
    if ! @scope_exits[scope_node] || ! @scope_exits[scope_node][scope_context_entry]
      #puts " No exits recorded"
      return []
    end
    #puts " scope context entry: #{scope_context_entry}"
    #puts " matching scope exit locations: #{@scope_exits[scope_node][scope_context_entry].to_a.join(", ")}"
    @scope_exits[scope_node][scope_context_entry]
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
    opts[:branch_delay_slots] ||= opts[:delay_slots]
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
        elsif current_instruction.branches?
          current_node.last_index = index + opts[:branch_delay_slots]
          index = current_node.last_index + 1
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
#  For efficiency reasons, we do not use qnames here
#  That is, CfgNodes are compared by pointer equality
class CfgNode

  attr_reader :vcfg, :nid, :successors, :predecessors, :context_tree

  def initialize(vcfg)
    @vcfg, @nid, @successors, @predecessors = vcfg, vcfg.nodes.size, [], []
#    @qname = "#{@vcfg.qname}/#{@nid}"
    @vcfg.nodes.push(self)
    @context_tree = ContextNode.new(:root)
  end

  def add_successor(node)
    @successors.push(node)
    node.predecessors.push(self)
  end

  def set_index(ix)
    @nid = ix
  end

  def add_context(ctx)
    node = @context_tree
    ctx.each_item do |item|
      node = node.add_child(item)
    end
  end
  # if this is a scope entry (call, loop-enter), matching scope exits in the given context
  def matching_scope_exits(cfmodel, location); [] ; end
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
  def successors_with_context(cfmodel, location)
    Enumerator.new do |ss|
      @successors.each { |s| ss << cfmodel.location(s, location.context) }
    end
  end
end

class BlockSliceNode < CfgNode
  attr_reader :block, :first_index
  attr_accessor :last_index
  def initialize(vcfg, block, first_ins, last_ins = -1)
    super(vcfg)
    @block, @first_index, @last_index = block, first_ins, last_ins
  end
  def successors_with_context(cfmodel, location)
    Enumerator.new do |ss|
      @successors.each { |s| ss << cfmodel.location(s, cfmodel.ctx_manager.blockslice(location.context, self)) }
    end
  end
  def instructions
    @block.instructions[@first..@last]
  end
  def to_s
    "#<BlockSliceNode #{block} #{first_index} #{last_index}>"
  end
end

class CallNode < CfgNode
  attr_reader :callsite
  def initialize(vcfg, callsite)
    super(vcfg)
    @callsite = callsite
  end

  # Note: call nodes change scope
  # scope node: entry of the called function's vcfg
  # scope entry scope-context: scope part of (context + callsite)
  def successors_with_context(cfmodel, location)
    Enumerator.new do |ss|
      # add call to context
      callee_context = cfmodel.ctx_manager.push_call(location.context, self)

      # yield locations at callee's entries
      callees(location).each { |vcfg|
        cfmodel.record_scope_entry(vcfg.entry, callee_context.scope_context, location.context)
        ss << cfmodel.location(vcfg.entry, callee_context)
      }
    end
  end

  # Enumerations locations which exit the scope entered at this location
  #
  def matching_scope_exits(cfmodel, location)
    Enumerator.new do |ss|
      # add call to context
      scope_entry_context = cfmodel.ctx_manager.push_call(location.context, self).scope_context

      callees(location).each { |vcfg|
        cfmodel.matching_scope_exits(vcfg.entry, scope_entry_context).each { |location|
          ss << location
        }
      }
    end
  end

  #
  # return VCFGs of functions possible called at this location
  #
  def callees(location)
    if location.node.callsite.unresolved_call?
      raise Exception.new("Indirect calls not yet supported: #{loc.node.callsite}")
    end
    Enumerator.new do |ss|
      callsite.callees.each { |flabel|
        ss << location.cfmodel.by_label(flabel)
      }
    end
  end

  def callnode? ; true; end
  def to_s
    "#<CallNode #{callsite}>"
  end
end

class ExitNode < CfgNode
  def initialize(vcfg) ; super(vcfg) ; end
  def exit? ; true ; end
  def successors_with_context(cfmodel, location)
    Enumerator.new do |ss|

      # get return context
      return_context, callnode = cfmodel.ctx_manager.pop_call(location.context)
      scope_entry_context = location.context.scope_context

      # handle program exit
      next if ! return_context

      # record scope exit
      cfmodel.record_scope_exit(vcfg.entry, scope_entry_context, location)

      # for all registered scope entry contexts
      cfmodel.matching_scope_entries(vcfg.entry, scope_entry_context).each { |scope_context_before|
        refined_context = return_context.with_scope_context(scope_context_before)
        callnode.successors.each { |s| ss << cfmodel.location(s, refined_context) }
      }
    end
  end
  def to_s
    "#<ExitNode #{vcfg.function}>"
  end
end

class LoopStateNode < CfgNode
  attr_reader :loop, :action
  # action is one out of :enter, :exit, :cont
  def initialize(vcfg, loop, action)
    super(vcfg)
    @loop, @action = loop, action
  end

  def successors_with_context(cfmodel, location)
    Enumerator.new do |ss|
      case action
      when :enter
        next_context = cfmodel.ctx_manager.enter_loop(location.context, self)
        scope_entry_context = next_context.scope_context
        cfmodel.record_scope_entry(self, scope_entry_context, location.context.scope_context)
        successors.each { |s| ss << cfmodel.location(s, next_context) }
      when :cont
        next_context = cfmodel.ctx_manager.continue_loop(location.context)
        successors.each { |s| ss << cfmodel.location(s, next_context) }
      when :exit
        # get return context
        exit_context, loopnode = cfmodel.ctx_manager.exit_loop(location.context)
        scope_entry_context = cfmodel.ctx_manager.reset_loop(location.context, loopnode).scope_context
        # record scope exit
        cfmodel.record_scope_exit(loopnode, scope_entry_context, location)
        # for all registered scope entry contexts
        cfmodel.matching_scope_entries(loopnode, scope_entry_context).each { |scope_context_before|
          refined_context = exit_context.with_scope_context(scope_context_before)
          successors.each { |s| ss << cfmodel.location(s, refined_context) }
        }
      else
        raise Exception.new("Bad loop state action")
      end
    end
  end
  def matching_scope_exits(cfmodel, location)
    return [] unless @action == :enter
    Enumerator.new do |ss|
      # get scope entry context
      scope_entry_context = cfmodel.ctx_manager.enter_loop(location.context, self).scope_context
      cfmodel.matching_scope_exits(self, scope_entry_context).each { |location| ss << location }
    end
  end
  def to_s
    "#<LoopStateNode #{loop} #{action}>"
  end
end


# Context node for storing context-sensitive information
class ContextNode
  def initialize(item)
    @item, @children = item, {}
  end
  def add_child(item)
    if c = @children[item]
      c
    else
      @children[item] = ContextNode.new(item)
    end
  end
  def dump(io=$stdout, level = 0)
    io.puts " "*(level*2) + @item.to_s
    @children.each { |key,node| node.dump(io, level+1) }
  end
end

# Locations are pairs of VCFG nodes and context
class Location
  attr_reader :cfmodel, :node, :context
  def initialize(cfmodel, node, context)
    @cfmodel, @node, @context = cfmodel, node, context
  end
  def successors
    @node.successors_with_context(@cfmodel, self)
  end
  def matching_scope_exits
    @node.matching_scope_exits(@cfmodel, self)
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

#
# Simple abstract interpretation on flat, inter-procedural control-flow model
# Following state-of-the-art AI frameworks, we separate abstract values and
# locations (which are an abstraction of program counter and history) for efficiency
# reasons
#
class Interpreter
  attr_reader :steps
  def initialize(semantics, cfmodel)
    @semantics, @cfmodel, @ctx_manager = semantics, cfmodel, cfmodel.ctx_manager
    @steps = 0
  end
  def interpret(initial_node, start_value)
    @in = {}
    @callcontexts, @returncontexts, @loopcontexts = {}, {}, {}
    initial_loc = Location.new(@cfmodel, initial_node, @ctx_manager.initial)
    @in[initial_loc] = start_value
    @queue = [initial_loc]
    while ! @queue.empty?
      @steps += 1
      loc = @queue.pop
      inval = @in[loc]
      outval  = @semantics.transfer_value(loc.node, inval)
      loc.successors.each { |loc| 
        if change = @semantics.merge(@in[loc],outval)
          @in[loc] = change[1]
          @queue.unshift(loc)
        end
      }
      # enqueue matching scope exit nodes (lazy construction of context-sensitive callgraph)
      loc.matching_scope_exits.each { |loc| @queue.unshift(loc) }
    end
    @in
  end

  def inputs_by_node
    by_node= {}
    @in.each { |loc,value| (by_node[loc.node] ||={})[loc.context] = value }
    by_node
  end

  def stats(io=$stdout)
    io.puts "Nodes: #{@cfmodel.vcfgs.values.map { |cfg| cfg.nodes.length }.inject(0,:+)}"
    io.puts "Locations: #{@in.keys.size}"
    io.puts "Steps: #{steps}"
  end

  def dump(io=$stdout)
    inputs_by_node.each do |node,ctxs|
      io.puts " NODE: #{node}#{" #{node.block}" if node.respond_to?(:block)}"
      ctxs.each do |ctx,val|
        io.puts "  CONTEXT: #{ctx}"
        io.puts "  VALUE: #{val}"
      end
    end
  end
end

# Top / Bottom
TOP=:top
BOTTOM=:bottom

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
