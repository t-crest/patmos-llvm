#
# platin tool set
#
# == Scope graphs
#
# A scope graph is a DAG consisting of functions and single-entry regions, the
# latter comprising loops and (future work) SESE regions.
#
# Scope graphs are useful for a variety of purposes, in particular for
# "Cache Persistence Analysis via Conflict-Free Region Identification"
#
# Note: Subfunctions either complete contain a loop or not all. Otherwise,
# they wouldn't be single entry.

require 'core/pml'

module PML

class ScopeGraph

  class Node
    include QNameObject
    attr_reader :context, :predecessors, :successors
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
    def entry
      @function.entry_block
    end
    def blocks
      @function.blocks
    end
    def to_s
      "F:#{function}#{context.qname}"
    end
  end

  class LoopNode < Node
    attr_reader :function,:loop
    def initialize(loop, context)
      super("#L#{loop.qname}", context)
      @function = loop.function
      @loop = loop
    end
    def entry
      @loop.loopheader
    end
    def blocks
      @loop.blocks
    end
    def to_s
      "L:#{loop}#{context.qname}"
    end
  end

  class RegionNode < Node
    attr_reader :function, :entryblock, :blocks
    def initialize(entryblock, blocks, context)
      super("#R#{entryblock}", context)
      @function = entryblock.function
      @entryblock, @blocks = entryblock, blocks
    end
    def to_s
      "R:#{entryblock}#{context.qname}"
    end
  end

  class CallSiteNode < Node
    attr_reader :function, :callsite
    def initialize(callsite, context)
      super("#C#{callsite.qname}", context)
      @function = callsite.function
      @callsite = callsite
    end
    def to_s
      "C:#{callsite}#{context.qname}"
    end
  end

  attr_reader :nodes
  def initialize(entry_function, refinement, pml, options)
    @pml, @options = pml, options
    @entry_function = entry_function
    @refinement = refinement
    build(@entry_function, options.callstring_length)
  end

  # get node for the specified loop
  def get_loop_node(loop, context)
    @nodeset[LoopNode.new(loop, context)]
  end

  # bottom-up traversal (note that scope graphs are acyclic)
  def bottom_up
    Enumerator.new { |ss|
      topological_order.reverse.each { |n| ss << n }
    }
  end

  # Topological sort for connected, acyclic graph
  # Concise implementation of a beautiful algorithm (Kahn '62)
  def topological_order
    return @topo if @topo
    @topo, worklist, vpcount = [], [@entry_node], Hash.new(0)
    while ! worklist.empty?
      node = worklist.pop
      @topo.push(node)
      node.successors.each { |n|
        vc = (vpcount[n] += 1)
        if vc == n.predecessors.length
          vpcount.delete(n)
          worklist.push(n)
        end
      }
    end
    assert("topological_order: not all nodes marked") { vpcount.empty? }
    @topo
  end

  def root
    @entry_node
  end

private
  def build(entry_function, calldepth)
    @nodes = []
    @nodeset = {}
    @visited = {}
    @calldepth = calldepth
    @entry_node = add_node(FunctionNode.new(entry_function, Context.empty))
    @worklist = [@entry_node]
    while ! @worklist.empty?
      fn = @worklist.pop
      next if @visited[fn]
      build_function(fn)
    end
  end
  def build_function(node)
    @visited[node] = true
    node.function.blocks.each { |b|
      # if block is infeasible, ignore it
      next if @refinement.infeasible_block?(b, node.context)
      # if block is in a loop, create loop node if necessary
      lnode = build_loop_node_for_block(node, b)
      bnode = add_node(RegionNode.new(b,[b],lnode.context))
      lnode.add_successor(bnode)
      b.callsites.each { |c|
        raise Exception.new("unresolved call") if c.unresolved_call?
        cnode = add_node(CallSiteNode.new(c,bnode.context))
        bnode.add_successor(cnode)
        @refinement.calltargets(c, node.context).each { |f|
          callee_node = add_node(FunctionNode.new(f, Context.empty)) # XXX: callstrings
          cnode.add_successor(callee_node)
          @worklist.push(callee_node)
        }
      }
    }
  end
  def build_loop_node_for_block(function_node, b)
    return function_node if b.loops.empty?
    this_loop = b.loops.first
    ctx = function_node.context
    innermost_loop_node = get_loop_node(this_loop, ctx)
    if innermost_loop_node.nil?
      parent_node = function_node
      b.loops.reverse.each { |loop|
        current_node = get_loop_node(loop,ctx)
        if current_node.nil?
          current_node = add_node(LoopNode.new(loop, ctx))
          parent_node.add_successor(current_node)
        end
        parent_node = current_node
      }
      innermost_loop_node = parent_node
    end
    innermost_loop_node
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

end # module PML
