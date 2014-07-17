#
# platin tool set
#
# Scopegraphs and Callgraphs

require 'core/pml'
require 'set'
module PML

#
# Helper Graph for building acyclic graphs from strongly-connected components
class SCCGraph
  class Node
    include QNameObject
    attr_writer :has_backedge
    attr_reader :first, :blocks, :successors, :node_of_block
    def initialize(blocks, graph_headers)
      @first = blocks.first
      @blocks = blocks
      @graph_headers = graph_headers
      @qname = "SCC: #{@first.qname}"
      @successors = []
      @has_backedge = false
    end
    def add_successor(n)
      @successors.push(n)
    end
    def trivial?
      return false if blocks.length > 1
      singleton = blocks.first
      singleton.successors.all? { |succ| succ != singleton || @graph_headers.include?(succ) }
    end
    def has_backedge?
      @has_backedge
    end
    def may_return?
      blocks.any? { |block| block.may_return? }
    end
    def to_s
      "SCCGraph::Node: #{blocks.join(", ")}"
    end
  end

  attr_reader :nodes, :recursive_edges
  def initialize(blocks, graph_headers)
    @blocks, @graph_headers = blocks, graph_headers
    @blockset = Set[*@blocks]

    # Attention: if flowfacts prune backedges, there might be LLVM loops which are not loops
    # in the sense of the SCC graph
    @sccs = TSortAdapter.new(blocks, graph_headers).strongly_connected_components.reverse

    @node_of_block = {}
    @nodes = @sccs.map { |scc| Node.new(scc, graph_headers) }
    nodes.each { |node|
      node.blocks.each { |b|
        @node_of_block[b] = node
      }
    }
    nodes.each { |node|
      node.blocks.each { |block|
        block.successors.each { |succ|
          next unless @blockset.include?(succ)
          if @graph_headers.include?(succ)
            node.has_backedge = true
            next
          end
          assert("Unknown node in SCC: #{succ}") { @node_of_block[succ] }
          if @node_of_block[succ] != node
            node.add_successor(@node_of_block[succ])
          end
        }
      }
    }
  end
  def dump(io = $stdout)
    puts("SCC Graph")
    puts "SCCs: #{@sccs.inspect}"
    nodes.each { |node|
      puts "- #{node}"
      node.successors.each { |succ|
        puts "    => #{succ.first}"
      }
    }
  end
end

#
# A scope graph is a hierarchical control-flow representation, generalizing
# the concept of a call graph. In contrast to a callgraph, it is acyclic.
#
# It consists of
#  * Function Nodes (representing the execution of a function in a certain context)
#    The only child of a function node is its body (a Control-Flow DAG)
#  * SCC Nodes (representing the execution of a strongly-connected component)
#    The children of a SCC node are functions (recursion) or a Control-Flow DAG (loops)
#  * Region Nodes (representing the execution of regular set of finite basic block sequences)
#  * Callsites (representing the call of a function)
#  * (Recursive Callsites) not yet implemented
#
# Scope graphs are useful for a variety of purposes, in particular for cache persistence analysis
#
class ScopeGraph

  # Scopegraph node (has a qualified name)
  class Node
    include QNameObject
    attr_reader :context, :predecessors, :successors, :region
    def initialize(name, context)
      @qname, @context = "#{name}#{context.qname}", context
      @predecessors, @successors = [], []
    end
    def add_successor(n)
      @successors.push(n)
      n.predecessors.push(self)
    end
  end

  class FunctionNode < Node
    attr_reader :function
    def initialize(function, context)
      super("#F#{function.qname}", context)
      @function = function
    end
    def scope_entry
      @function
    end
    def region=(region_node)
      assert("FunctionNode: region already set") { @region.nil? }
      @region = region_node
    end
    def to_s
      "F:#{function}#{context.qname}"
    end
    def inspect
      to_s
    end
  end

  # For SCCs in the callgraph (not yet implemented) and loop nodes
  class SCCNode < Node
    def initialize(name, context)
      super(name, context)
    end
  end

  class LoopNode < SCCNode
    attr_reader :loop
    def initialize(loop, context)
      super("#L#{loop.qname}", context)
      @loop = loop
    end
    def scope_entry
      @loop
    end
    def region=(region_node)
      assert("LoopNode: region already set") { @region.nil? }
      @region = region_node
    end
    def to_s
      "L:#{loop}#{context.qname}"
    end
  end

  class BlockNode < Node
    attr_reader :block
    def initialize(block, context)
      super("#B#{block.qname}", context)
      @block = block
    end
    def scope_entry
      @block
    end
    def to_s
      "B:#{@block}#{context.qname}"
    end
  end

  class CallNode < Node
    attr_reader :function, :callsite

    def initialize(callsite, context)
      super("#C#{callsite.qname}", context)
      @function = callsite.function
      @callsite = callsite
    end
    def scope_entry
      @callsite
    end
    def to_s
      "C:#{callsite}#{context.qname}"
    end
  end

  attr_reader :nodes

  #
  # build scope graph rooted at the specified +entry_function+. the
  # control-flow +refinement+ specifies infeasible blocks and calltargets
  # (possibly context-sensitive).
  #
  def initialize(entry_function, refinement, pml, options)
    assert("ScopeGraph#initialize: first argument is not a Function") { entry_function.kind_of?(PML::Function) }
    @pml, @options = pml, options
    @entry_function = entry_function
    @refinement = refinement
    @ctx_manager = ContextManager.create(options.callstring_length || 0)
    build(@entry_function)
  end

  # get function node for the specified function
  def get_function_node(function, context)
    @nodeset[FunctionNode.new(function, context)]
  end

  # get node for the specified loop
  def get_loop_node(loop, context)
    @nodeset[LoopNode.new(loop, context)]
  end

  # get call node for the specified callsite
  def get_call_node(callsite, context)
    @nodeset[CallNode.new(callsite, context)]
  end

  def root
    @entry_node
  end

  # bottom-up traversal (note that scope graphs are acyclic)
  def bottom_up
    Enumerator.new { |ss|
      topological_order.reverse.each { |n| ss << n }
    }
  end

  def topological_order
    return @topo if @topo
    @topo = topological_sort(@entry_node)
  end


private

  def build(entry_function)
    @nodes = []
    @nodeset = {}
    @visited = {}
    @entry_node = add_node(FunctionNode.new(entry_function, Context.empty))
    @worklist = [@entry_node]
    while ! @worklist.empty?
      fn = @worklist.pop
      next if @visited[fn]
      build_function(fn)
    end
  end

  #
  # Build the scopegraph and the collapsed region DAGs.
  #
  def build_function(node)
    @visited[node] = true

    # feasible blocks in the function
    function_blocks = node.function.blocks.select { |b|
      # if block is infeasible, ignore it
      ! @refinement.infeasible_block?(b, node.context)
    }
    # build SCCs
    build_regions(node, function_blocks)
  end

  #
  # Build a region of SCCs
  #
  def build_regions(function_node, blocks)
    assert("Function should have feasible blocks: #{function_node}") { ! blocks.empty? }
    @blockslices = []

    # process all loops
    blocks_of_node = { function_node => blocks }
    loop_of_node   = { function_node => nil }
    loopqueue = WorkList.new([function_node])

    loopqueue.process { |scope_node|
      loop = loop_of_node[scope_node]
      entry = loop ? loop.loopheader : function_node.function.entry_block

      # created a collapsed graph, excluding backedges
      scc_graph = SCCGraph.new(blocks_of_node[scope_node], [entry])

      # create a new region graph
      scope_node.region = region_graph = RegionGraph.new(self, entry.qname)

      # build region graph and add subscopes
      build_region_graph(region_graph, scc_graph, entry, loop, scope_node.context, blocks_of_node, loop_of_node)

      # expand scope graph
      region_graph.loop_nodes.each { |loop_node|
        add_node(loop_node)
        scope_node.add_successor(loop_node)
        loopqueue.enqueue(loop_node)
      }
      region_graph.call_nodes.each { |call_node|
        add_node(call_node)
        callsite = call_node.callsite
        scope_node.add_successor(call_node)
        # scope graph: process callsite target
        @refinement.calltargets(callsite, scope_node.context).each { |f|
          new_context = @ctx_manager.push_call(scope_node.context, callsite)
          callee_node = add_node(FunctionNode.new(f, new_context))
          call_node.add_successor(callee_node)
          @worklist.push(callee_node)
        }
      }
    }

    # correctness check
    slice_groups = @blockslices.group_by { |bs| bs.first.block }
    blocks.each { |b|
      current=-1
      slice_groups[b] ||= [] # empty blocks
      slice_groups[b].each { |bs|
        assert("Block slices not complete: #{b}") { bs.first.index == current+1 }
        current = bs.last.index
      }
      assert("Block slices not complete: #{b}") { current+1 == b.instructions.length }
    }
  end

  def build_region_graph(region_graph, scc_graph, entry, scope_loop, context, blocks_of_node, loop_of_node)
    entry_node, exit_node = {}, {}
    scc_graph.nodes.each { |scc_node|
      blocks = scc_node.blocks
      # distinguish whether SCC is trivial (block) or loop (subscope)
      if scc_node.trivial?

        assert("SCC should either be loop or have size 1") { blocks.length == 1 }
        block = blocks.first

        # region graph: block entry node
        region_node = region_graph.add_block_entry(block)
        entry_node[scc_node] = region_node
        next_index = 0

        block.callsites.each { |c|
          raise Exception.new("unresolved call (in function: #{block.function.label})") if c.unresolved_call?

          # block slice node
          first_ins, last_ins = [next_index, c.index + c.delay_slots].map { |ix| block.instructions[ix] }
          region_node = region_graph.add_block_slice(region_node, first_ins, last_ins)

          @blockslices.push(region_node)

          next_index = c.index + c.delay_slots + 1

          # subscope node for callsite
          callsite_node = CallNode.new(c, context)
          region_node = region_graph.add_callsite(region_node, callsite_node)
        }
        if next_index != block.instructions.length
          first_ins, last_ins = [next_index, block.instructions.length - 1].map { |ix| block.instructions[ix] }
          region_node = region_graph.add_block_slice(region_node, first_ins, last_ins)
          @blockslices.push(region_node)
        end
        exit_node[scc_node] = region_node

      else
        inner_loop = scc_node.first.loop

        # subscope node for loop
        loop_node = LoopNode.new(inner_loop, context)
        region_node = region_graph.add_subscope(loop_node)
        entry_node[scc_node] = exit_node[scc_node] = region_node

        # save mapping for later
        blocks_of_node[loop_node] = scc_node.blocks
        loop_of_node[loop_node]   = inner_loop
      end
      if blocks.include?(entry)
        region_graph.entry_node.add_successor(entry_node[scc_node])
      end
    }

    scc_graph.nodes.each { |scc_node|
      scc_node.successors.each { |succ_scc_node|
        source = exit_node[scc_node]
        target = entry_node[succ_scc_node]
        source.add_successor(target)
      }
      if scc_node.has_backedge?
        exit_node[scc_node].add_successor(region_graph.backedge_node(entry))
      end
      if scc_node.may_return?
        exit_node[scc_node].add_successor(region_graph.exit_node)
      end
    }
  end

  def add_node(n)
    if existing = @nodeset[n]
      existing
    else
      @nodeset[n] = n
      @nodes.push(n)
      n
    end
  end
end

# DAG that represents control-flow in a single-entry region
class RegionGraph

  # RegionGraph node
  class Node
    include QNameObject
    attr_reader :name, :predecessors, :successors
    def initialize(name)
      @qname = name
      @predecessors, @successors = [], []
    end
    def add_successor(n)
      @successors.push(n)
      n.predecessors.push(self)
    end
    def dup_node
      copy = self.dup
      copy.reset_edges!
      copy
    end
    def reset_edges!
      @successors = []
      @predecessors = []
    end
  end

  class EntryNode < Node
    def initialize
      super("EntryNode")
    end
    def to_s
      "EntryNode"
    end
  end

  class ExitNode < Node
    def initialize
      super("ExitNode")
    end
    def to_s
      "ExitNode"
    end
  end

  class BlockEntryNode < Node
    attr_reader :block
    def initialize(block)
      super(block.qname)
      @block = block
    end
    def to_s
      "BlockEntryNode: #{@block}"
    end
  end

  class BlockSliceNode < Node
    attr_reader :block, :first, :last
    def initialize(block, first, last)
      super(block.qname+"[#{first}..#{last}]")
      @block = block
      @first, @last = first, last
    end
    def to_s
      "BlockSliceNode: #{@block}[#{first}..#{last}]"
    end
  end

  class SubScopeNode < Node
    attr_reader :scope_node
    def initialize(scope_node)
      super(scope_node.qname)
      @scope_node = scope_node
    end
    def to_s
      "SubScope: #{scope_node}"
    end
  end

  class ActionNode < Node
    attr_reader :action, :blockslice
    def initialize(action, blockslice)
      super(action.to_s)
      @action, @blockslice = action, blockslice
    end
    def block
      blockslice.block
    end
    def to_s
      "ActionNode: #{action}"
    end
  end

  class RecNode < Node
    def initialize(target)
      super("RecNode: #{target.qname}")
      @target = target
    end
    def to_s
      "RecNode: #{@target}"
    end
  end

  attr_reader :region, :nodes, :name, :entry_node
  def initialize(scopegraph, name)
    @scopegraph = scopegraph
    @name = name.to_s
    @nodes = []
    @nodeset = {}
    @entry_node = add_node(EntryNode.new)
  end

  # build a new region graph, with additional block action nodes inserted after block slice nodes
  def action_graph
    arg = RegionGraph.new(@scopegraph, @name)
    entry_node, exit_node = {}, {}
    self.nodes.each { |n|
      current_node = arg.add_node(n.dup_node)
      entry_node[n] = current_node
      if current_node.kind_of?(BlockSliceNode)
        block_node = current_node
        fst, lst = n.first.index, n.last.index
        fst.upto(lst) { |ix|
          actions = yield n.first.block.instructions[ix]
          actions.each { |action|
            current_node = arg.add_action_node(current_node, action, block_node)
          }
        }
      end
      exit_node[n] = current_node
    }
    self.nodes.each { |n|
      n.successors.each { |succ|
        exit_node[n].add_successor(entry_node[succ])
      }
    }
    arg
  end

  def entry_node=(entry_node)
    @entry_node = entry_node
  end

  def action_nodes
    nodes.select { |n | n.kind_of?(ActionNode) }
  end

  def subscopes
    nodes.select { |n| n.kind_of?(SubScopeNode) }.map { |n| n.scope_node }
  end

  def loop_nodes
    subscopes.select { |n| n.kind_of?(ScopeGraph::LoopNode) }
  end

  def call_nodes
    subscopes.select { |n| n.kind_of?(ScopeGraph::CallNode) }
  end

  def to_s
    "RegionGraph: #{@name}"
  end

  def add_block_entry(block)
    add_node(BlockEntryNode.new(block))
  end

  def add_block_slice(predecessor_node, first_ins, last_ins)
    block_slice_node = add_node(BlockSliceNode.new(first_ins.block, first_ins, last_ins))
    predecessor_node.add_successor(block_slice_node)
    block_slice_node
  end

  def add_action_node(predecessor_node, action, blockslice)
    action_node = add_node(ActionNode.new(action, blockslice))
    predecessor_node.add_successor(action_node)
    action_node
  end

  def add_callsite(predecessor_node, callsite_node)
    snode = add_subscope(callsite_node)
    predecessor_node.add_successor(snode)
    snode
  end

  def add_subscope(scope_node)
    add_node(SubScopeNode.new(scope_node))
  end

  def backedge_node(target)
    rec_node = add_node(RecNode.new(target))
    rec_node.add_successor(exit_node)
    rec_node
  end

  def exit_node
    add_node(ExitNode.new)
  end

  def dump(io=$stdout)
    puts "Dumping RegionGraph with #{@nodes.length} nodes"
    @nodes.each { |n|
      puts "- #{n}"
      n.successors.each { |s| puts "  --> #{s}" }
    }
    require 'graphviz'
    g = GraphViz.new(:G, :type => :digraph)
    g.node[:shape] = "rectangle"
    g[:label] = "Region Graoh #{name}"
    node_dict, nids = {}, {}
    nodes.each_with_index { |n,i| nids[n] = i }
    nodes.each { |node|
      nid = nids[node]
      label = node.to_s
      node_dict[node] = g.add_nodes(nid.to_s, :label => label)
    }
    nodes.each { |n|
      n.successors.each { |s|
        g.add_edges(node_dict[n],node_dict[s])
      }
    }
    file = "tmp/region_#{name.gsub('/','_')}.eps"
    puts "DEBUG file: #{file}"
    g.output( :eps => file)
  end

  def add_node(n)
    if existing = @nodeset[n]
      existing
    else
      @nodes.push(n)
      @nodeset[n] = n
    end
  end

private

end

# Callgraph are a specialization of scope graphs; they only consist of function nodes, edges
# are labelled with callsites
class CallGraph < PMLObject
  attr_reader :nodes
  class FunctionNode < PMLObject
    attr_reader :successors_with_callsite
    def initialize(function, context)
      @function, @context = function, context
      @qname= "CGNode:#{function.qname}#{context.qname}"
      @successors_with_callsite = []
    end
    def add_callsite(callsite, targets)
      targets.each { |t|
        @successors_with_callsite.push([t,callsite])
      }
    end
    def successors
      Set[*@successors_with_callsite.map { |t,cs| t }].to_a
    end
    def to_s
      "CGNode:#{@function}#{@context.qname}"
    end
  end
  def initialize
    @nodes = []
  end
  def dump(io=$stdout)
    io.puts "Callgraph"
    @nodes.each { |n|
      io.puts " Node #{n}"
      n.successors_with_callsite.each { |t,cs|
        io.puts "   #{cs}->#{t}"
      }
    }
  end
  def add_node(function, context)
    n = FunctionNode.new(function, context)
    @nodes.push(n)
    n
  end
end


class ScopeGraph
  # get callgraph (scopegraph restricted to FunctionNodes) for scopegraph
  def callgraph
    nodes, edges = Set.new, []
    worklist = WorkList.new([ [self.root, self.root] ])
    worklist.process { |node, function_node|
      assert("ScopeGraph#callgraph: function_node of wrong type") { function_node.kind_of?(FunctionNode) }
      nodes.add(node) if node.kind_of?(FunctionNode)
      node.successors.each { |successor_node|
        if(successor_node.kind_of?(FunctionNode))
          edges.push([function_node,successor_node,node])
          worklist.enqueue([successor_node, successor_node])
        else
          worklist.enqueue([successor_node, function_node])
        end
      }
    }
    cg = CallGraph.new
    cg_nodes = {}
    nodes.each { |n|
      cg_nodes[n] = cg.add_node(n.function, n.context)
    }
    edges.each { |pre,succ,cs|
      cg_nodes[pre].add_callsite(cs, [ cg_nodes[succ] ])
    }
    cg
  end
end


end # module PML
