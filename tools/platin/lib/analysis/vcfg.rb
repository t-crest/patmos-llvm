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
  def initialize(function_list, entry, flowfacts, ctx_manager, arch, opts = {})
    @function_list, @entry, @flowfacts, @ctx_manager, @arch, @opts = function_list, entry, flowfacts, ctx_manager, arch, opts.dup
    @vcfgs = { @entry => VCFG.new(@entry, arch) }
    @locations, @scope_entries, @scope_exits = {}, {}, {}
    extract_flow_refinements

    # compute all reachable contexts
    ip = Interpreter.new(ReachabilitySemantics.new, self)
    ip.interpret(get_vcfg(entry).entry, true)
    ip.stats($stdout)
    ip.inputs_by_node.each do |node,ctxs|
      ctxs.keys.each do |ctx|
        node.context_tree.set(ctx.to_a.reverse,:reachable)
      end
      # puts node
      # node.context_tree.dump($stdout,1)
    end
  end
  #
  # get or create the VCFG for the given function
  #
  def get_vcfg(f)
    vcfg = @vcfgs[f]
    unless vcfg
      vcfg = @vcfgs[f] = VCFG.new(f, @arch)
    end
    vcfg
  end

  #
  # get VCFG by label of the represented function
  #
  def by_label(label)
    get_vcfg(@function_list.by_label(label))
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
private
  #
  # Refine control-flow model using infeasible/calltarget flowfact information
  #
  # This method implements two refinements:
  #
  # (1) in global scope, frequency==0 => dead code (infeasible)
  # (2) in global scope, dynamic != static receiver set => refine calltarget sets
  #
  def extract_flow_refinements
    @flowfacts.each do |ff|
      # set indirect call targets
      scope,cs,targets = ff.get_calltargets
      if scope && scope.programpoint.kind_of?(Function) && scope.programpoint == @entry && scope.any_context?
        target_vcfgs = targets.map { |fref| get_vcfg(fref.function) }
        get_callnode(cs.instruction).refine_calltargets(to_bounded_stack(cs.context), target_vcfgs)
      end
      # set infeasible blocks
      scope,bref = ff.get_block_infeasible
      if scope && scope.programpoint.kind_of?(Function) && scope.programpoint == @entry && scope.any_context?
        get_vcfg(bref.function).get_blockstart(bref.block).set_infeasible(to_bounded_stack(bref.context))
      end
    end
  end
  def get_callnode(cs)
    get_vcfg(cs.function).get_callnode(cs)
  end
  def to_bounded_stack(callstring)
    BoundedStack.create(callstring.callsites.map { |csref| get_callnode(csref.instruction) })
  end
end

#
# Virtual CFG, consists of Entry, Exit, Call and BlockSlice nodes
class VCFG
  include QNameObject

  attr_reader :function, :nodes, :entry, :exit

  def initialize(function, arch)
    @function = function
    @qname = "V#{@function.qname}"
    @nodes = []
    @entry = EntryNode.new(self)
    @exit = ExitNode.new(self)

    # create nodes for all basic blocks slices and call / return sites
    @blockstart, block_predecessors, @callnodes = build_basic_block_nodes(function, arch)

    # Link nodes in the VCFG
    @entry.add_successor(@blockstart[function.blocks.first])
    function.blocks.each do |succblock|
      succnode  = @blockstart[succblock]
      assert("No node matching block #{succblock}") { succnode }
      (block_predecessors[succblock]||[]).each do |prednode|
        predblock = prednode.block
        # if successors is in a different loop then predecessor, we need to insert loop exit nodes
        if predblock.loopnest > 0 && succblock.loops.first != predblock.loops.first
          loopix = 0
          # exit loops until successor level is equal and matching, or smaller
          while predblock.loopnest - loopix >= succblock.loopnest && predblock.loops[loopix] != succblock.loops.first
            prednode = insert_loop_node(prednode, predblock.loops[loopix], :exit)
            loopix += 1
          end
        end
        # if successor is a loop header, we insert loop enter/cont nodes
        if succblock.loopheader?
          if predblock.loops.include?(succblock.loop) # continue
            prednode = insert_loop_node(prednode, succblock, :cont)
          else
            prednode = insert_loop_node(prednode, succblock, :enter)
          end
        end
        prednode.add_successor(succnode)
      end
    end
  end
  def get_callnode(callsite)
    assert("expecting instruction, not #{callsite.class}") { callsite.kind_of?(Instruction) }
    @callnodes[callsite]
  end
  def get_blockstart(block)
    assert("expecting block, not #{block.class}") { block.kind_of?(Block) }
    @blockstart[block]
  end
private
  def build_basic_block_nodes(function, arch)
    first_nodes = {}
    block_predecessors = {}
    callnodes = {}
    function.blocks.each do |block|
      if(block.instructions.size == 0)
        first_nodes[block] = blocknode = BlockSliceNode.new(self, block, 0, -1)
        block.successors.each { |b| add_block_predecessor(block_predecessors, b, blocknode) }
        next
      end
      has_branchtarget, has_return = false, false
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
          index = get_split_index(current_node, block, index)
          call_node = callnodes[current_instruction] = CallNode.new(self, current_instruction)
          current_node.add_successor(call_node)
          split_node = call_node
        elsif current_instruction.returns?
          has_return = true
          index = get_split_index(current_node, block, index)
          current_node.add_successor(@exit)
          split_node = current_node
        elsif current_instruction.branches?
          has_branchtarget = true
          index = get_split_index(current_node, block, index)
          split_node = current_node
          current_instruction.branch_targets.each { |succblock|
            add_block_predecessor(block_predecessors,succblock,split_node)
          }
        else
          index += 1
        end
      end
      if block.successors.empty?
        # Either return or stuck node (should never be emitted by the compiler)
	# SH: could be a tail call to a noreturn function; check if node has a call
        if ! has_return
          # warn("Block with empty successors but without return information - adding exit edge")
          current_node.add_successor(@exit)
        end
      elsif ! has_branchtarget
        # Either no branch information available or no branches in the block
        # warn("No branch-target information available") if block.successors.size != 1
        block.successors.each { |b|
          add_block_predecessor(block_predecessors, b, current_node)
        }
      elsif b = block.fallthrough_successor
        # There was branch information available, and the block is fall-through
        add_block_predecessor(block_predecessors,b,current_node)
      end
      current_node.last_index = block.instructions.size - 1
    end
    return [first_nodes, block_predecessors, callnodes]
  end

  # compute index of instruction that should be the start of a new block after
  # a branch, call or return
  def get_split_index(current_node, block, current_index)
    ins = block.instructions[current_index]
    delay_slots_left = ins.delay_slots

    # find the bundle in the last delay slot
    while(delay_slots_left > 0)
      # find next bundle
      current_index += 1
      current_index += 1 while block.instructions[current_index].bundled?
      # decrement delay slot count
      delay_slots_left -= 1
    end

    # current index now points at the bundle for the last delay slot
    # find the last instruction in this bundle
    while (current_index+1) < block.instructions.length && block.instructions[current_index+1].bundled?
      current_index += 1
    end
    current_node.last_index = current_index
    return (current_node.last_index + 1)
  end

  def add_block_predecessor(dict, succblock, prednode)
    dict[succblock] ||= Set.new
    dict[succblock].add(prednode)
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
    @context_tree = ContextTree.new
  end

  def add_successor(node)
    @successors.push(node)
    node.predecessors.push(self)
  end

  def set_index(ix)
    @nid = ix
  end

  # if this is a scope entry (call, loop-enter), matching scope exits in the given context
  def matching_scope_exits(cfmodel, location); [] ; end
  def callnode? ; false ; end
  def instructions ; [] ; end
  def entry? ; false ; end
  def exit? ; false ; end
  def block ; nil ; end
  def block_start? ; false ; end
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
    @infeasible = ContextTree.new
  end
  def set_infeasible(callstring)
    @infeasible.set(callstring.to_a, true)
  end
  def successors_with_context(cfmodel, location)
    # check infeasibility
    return [] if @infeasible.path_find_first(location.context.callstring.to_a)
    Enumerator.new do |ss|
      @successors.each { |s| ss << cfmodel.location(s, cfmodel.ctx_manager.blockslice(location.context, self)) }
    end
  end
  def instructions
    @block.instructions[@first_index..@last_index]
  end
  def block_start?
    @first_index == 0
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
    @targets = ContextTree.new
  end

  def block
    @callsite.block
  end
  def block_start?
    # SH: Call nodes are always preceded by a BlockSliceNode.. (I think)
    false
  end

  def refine_calltargets(callstring, targetset)
    @targets.set(callstring.to_a, targetset)
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
    if ts = @targets.path_find_last(location.context.callstring.to_a)
      return ts
    end
    if location.node.callsite.unresolved_call?
      raise Exception.new("Indirect calls not yet supported: #{location.node.callsite}")
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
