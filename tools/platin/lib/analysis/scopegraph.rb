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

  class CFAdapter
    attr_reader :entry
    def initialize(entry)
      @entry = entry
      @predecessors, @successors = {}, {}
    end
    def successors(node)
      @successors[node] ||= []
    end
    def predecessors(node)
      @predecessors[node] ||= []
    end
    def add_edge(source,target)
      predecessors(target).push(source)
      successors(source).push(target)
    end
  end

  class RegionNode < Node
    attr_reader :function, :blocks, :entry_block

    def size
      @blocks.size
    end

    # adapter that provides control-flow successors and predecessors
    # for all scopegraph successors of this node
    def subgraph_adapter
      # (1) compute mapping from blocks to subscope
      block_container = {}
      successors.each { |subscope|
        subscope.blocks.each { |block|
          block_container[block] = subscope
        }
      }
      adapter = CFAdapter.new(block_container[entry_block])
      # (2) add edges
      successors.each { |subscope|
        subscope.blocks.each { |block|
          block.successors.each { |succblock|
            succscope = block_container[succblock]
            next unless succscope # infeasible block
            if succscope != subscope
              adapter.add_edge(subscope, succscope)
            end
          }
        }
      }
      adapter
    end
protected
    def initialize(name, context, function, entry_block, blocks)
      super(name, context)
      @function, @entry_block, @blocks = function, entry_block, blocks
    end
  end

  class FunctionNode < RegionNode
    attr_reader :function, :blocks
    def initialize(function, context, blocks = nil)
      super("#F#{function.qname}", context, function, function.entry_block, blocks || function.blocks)
    end

    def entry
      @function.entry_block
    end
    def to_s
      "F:#{function}#{context.qname}"
    end
  end

  class LoopNode < RegionNode
    attr_reader :loop
    def initialize(loop, context, blocks = nil)
      super("#L#{loop.qname}", context, loop.function, loop.loopheader, blocks || loop.blocks)
      @loop = loop
    end
    def entry
      @loop.loopheader
    end
    def to_s
      "L:#{loop}#{context.qname}"
    end
  end

  class BlockNode < Node
    attr_reader :function, :block
    def initialize(block, context)
      super("#B#{block}", context)
      @function = block.function
      @block = block
    end
    def blocks
      [block]
    end
    def to_s
      "B:#{block}#{context.qname}"
    end
    def subgraph_adapter
      adapter = CFAdapter.new(successors.first)
      pred = nil
      successors.each { |s|
        if pred
          adapter.add_edge(pred, s)
        end
        pred = s
      }
      adapter
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

  def topological_order
    return @topo if @topo
    @topo = topological_sort(@entry_node)
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
    @entry_node = add_function_node(entry_function, Context.empty)
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
      bnode = add_node(BlockNode.new(b,lnode.context))
      lnode.add_successor(bnode)
      b.callsites.each { |c|
        raise Exception.new("unresolved call") if c.unresolved_call?
        cnode = add_node(CallSiteNode.new(c,bnode.context))
        bnode.add_successor(cnode)
        @refinement.calltargets(c, node.context).each { |f|
          new_context = Context.empty  # XXX: callstrings
          callee_node = add_function_node(f, new_context)
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
          current_node = add_loop_node(loop, ctx)
          parent_node.add_successor(current_node)
        end
        parent_node = current_node
      }
      innermost_loop_node = parent_node
    end
    innermost_loop_node
  end

  def add_function_node(function, context)
    blocks = function.blocks.select { |fb| ! @refinement.infeasible_block?(fb, context) }
    add_node(FunctionNode.new(function, context, blocks))
  end

  def add_loop_node(loop, context)
    blocks = loop.blocks.select { |fb| ! @refinement.infeasible_block?(fb, context) }
    add_node(LoopNode.new(loop, context, blocks))
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
