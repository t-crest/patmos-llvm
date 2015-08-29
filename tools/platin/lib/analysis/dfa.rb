#
# platin tool set
#
# == Data Flow Analysis Framework
#

require 'core/pml'
require 'core/utils'

require 'set'

class LiftedType
end
class LiftedBOT < LiftedType
  def lub(rhs) ; rhs ; end
  def bot? ; true ; end
  def top? ; false; end
  def to_s ; "BOT"; end
  def dup  ; self; end
end
class LiftedTOP < LiftedType
  def lub(rhs) ; self ; end
  def bot? ; false; end
  def top? ; true ; end
  def to_s ; "TOP"; end
  def dup  ; self; end
end
class LiftedType
  BOT = LiftedBOT.new
  TOP = LiftedTOP.new
  attr_reader :value
  def initialize(value, lubop)
    @value, @op = value, lubop
  end
  def lub(rhs)
    return self if rhs.bot?
    return rhs  if rhs.top?
    LiftedType.new(@op.call(self.value,rhs.value),@op)
  end
  def top? ; false; end
  def bot? ; false; end
  def to_s ; end
  def dup  ; end
  def eq?(rhs)
    return false if rhs == TOP or rhs == BOT
    @value == rhs.value
  end
end

class DFAOperator
  # Return the initial (bottom) state for all nodes
  def init ; nil ; end
  # Return the state for the entry node
  def entry ; nil ; end
  # Return the joined state for a list of out states
  def join(outs) ; nil ; end
  # Transfer in state to out state.
  def transfer(node, ins) ; ins ; end
  # Check if state has changed
  def changed?(olds, news) ; olds != news ; end
end

class LiftedDFAOperator < DFAOperator
  def init ; LiftedType.BOT; end
  def join(outs) ; outs.reduce(:lub) ; end
end

class DFASetOperator < LiftedDFAOperator
  attr_reader :operator, :less
  # Less must take two states A and B and return true if A <= B
  def initialize(operator, less)
    @operator = operator 
    @less = less
  end
  def init ; Set.new ; end
  def entry ; Set.new([@operator.entry]) ; end
  def join(outs)
    merge( outs.reduce(Set.new) { |set,out| set.merge(out) } )
  end
  def transfer(node, ins)
    outs = ins.map {|i| @operator.transfer(node, i) }.to_set
    merge(outs)
  end
  def merge(outs)
    outs.delete_if { |s1| outs.any? { |s2| s1 != s2 && @less.call(s1, s2) } }
  end
end

class DataFlowAnalysis
  
  class Node
    # Determines the order in the worklist
    attr_accessor :order
    attr_reader :bundle
    attr_accessor :predecessors, :successors
    attr_accessor :outs
    def initialize(bundle = nil)
      @bundle = bundle
      @predecessors = []
      @successors = []
      @outs = nil
    end
    def entry? ; false ; end
    def exit?  ; false ; end
    def reachable? ; order >= 0; end
    def <=>(rhs)
      @order <=> rhs.order
    end
    def to_s
      "##{order} #{exit? ? 'T' : ' '}#{@bundle}: #{@outs}"
    end
  end
  class EntryNode < Node
    def entry? ; true ; end
  end
  class ExitNode < Node
    def exit?  ; true ; end
  end

  # TODO order matters here for performance. Use different worklist order based on DFA direction
  class Worklist
    def initialize
      #@worklist = []
      @worklist = SortedSet.new
    end
    def push(successors)
      #@worklist.concat(successors)
      @worklist.merge(successors)
    end
    def pop
      # Sort nodes in the worklist based on their topological order
      #@worklist.sort!{ |n1,n2| n1.order > n2.order }
      #@worklist.pop
      node = @worklist.first
      @worklist.delete(node)
      node
    end
    def empty? ; @worklist.empty? ; end
    def length ; @worklist.length ; end
    def include?(node) ; @worklist.include?(node) ; end
  end

  # @param blocks Subset of blocks to analyze, or nil to analyze whole function.
  def initialize(pml, options, function, entry_block, blocks =  nil, reverse = false)
    @pml, @options, @function, @entry_block, @blocks, @reverse = pml, options, function, entry_block, blocks, reverse
    @blocks ||= @function.blocks
    build_graph
  end

  def analyze(operator)
    worklist = Worklist.new
    init = operator.init
    # Initialize sets
    @nodes.each { |n|
      n.outs = init
    }
    # Get out set of entry node
    @entry_node.outs = operator.entry

    # Ensure that all nodes are transformed at least once
    worklist.push(@nodes.select { |n| n.reachable? and not n.entry?} )

    # Run worklist algorithm
    step_count = 0
    while not worklist.empty?
      node = worklist.pop
      
      ins  = operator.join( node.predecessors.map{|p|p.outs} )
      outs = operator.transfer( node, ins )
      changed = operator.changed?( node.outs, outs )

      node.outs = outs if changed
      worklist.push(node.successors) if changed

      # Debug
      step_count += 1
      debug(@options,:dfa) {
        "DFA step #{step_count} @ #{node.bundle}: #{ins} -> #{outs} #{changed ? 'CHANGED' : ''}"
      }
    end

    #dump(worklist,step_count)

    @exit_nodes
  end

  def build_graph
    @exit_nodes = []
    @entry_node = EntryNode.new
    @nodes = [@entry_node]
    
    pred_nodes = {}
    first_node = {}

    # Build nodes
    @blocks.each { |b|
      # add all instruction bundles except the last one and link them
      # TODO this code is not quite correct: We assume here that there is only
      #      one branch instruction in the block. We should identify the targets
      #      of the instruction (respecting delay slots!) and set the
      #      targets correctly per instruction
      targets = b.successors.select { |s| @blocks.include?(s) }
      last = nil
      bundles = b.bundles
      bundles[0..-2].each { |i|
        node = Node.new(i)
	add_node(b, node, last, first_node)
	last = node
      }
      if bundles.empty?
	node = Node.new
      else
        # Is this a return block or does the block have a sucessor outside the region?
	if b.successors.empty? or targets.length < b.successors.length
	  # Then we have an exit node
	  node = ExitNode.new(bundles.last)
	  @exit_nodes.push(node)
	else
	  node = Node.new(bundles.last)
	end
      end
      add_node(b, node, last, first_node, targets, pred_nodes)
    }
    
    # Add edges between blocks
    pred_nodes.each { |b,preds| 
      first_node[b].predecessors = preds
    }
    # Add entry edge
    first_node[@entry_block].predecessors.push(@entry_node)

    # Add successor edges
    @nodes.each { |n|
      n.predecessors.each { |pred| pred.successors.push(n) }
    }
    
    if @reverse
      # TODO need to reverse entry and exit as well!
      nodes.each { |n|
        pred = n.predecessors
	n.predecessors = n.successors
	n.successors = pred
      }
    end

    topological_sort
  end

  def topological_sort
    @nodes.each { |n| n.order = -1 }
    postorder = []
    topo_visit(@entry_node, postorder)
    postorder.reverse_each.with_index { |n,idx| n.order = idx }
    @entry_node.order = 0
  end
  def topo_visit(node, postorder)
    # Found a cycle if node.order == -2
    return if node.order < -1
    # temporary mark
    node.order = -2
    node.successors.each { |s| topo_visit(s, postorder) }
    # permanent mark
    node.order = -3
    postorder.push(node)
  end

  def add_node(b, node, last, first_node, targets = [], pred_nodes = nil)
    node.predecessors.push(last) if last
    first_node[b] = node if not last
    @nodes.push(node)
    targets.each { |s|
      pred_nodes[s]||=[]
      pred_nodes[s].push(node)
    }
  end

  def dump(worklist, step)
    puts "DFA Step #{step}, Worklist size #{worklist.length}:"
    @nodes.each { |node| 
      puts "  #{worklist.include?(node)?'*':' '}#{node.exit? ? 'T':' '} ##{node.order} #{node.bundle}: #{node.outs}"
    }
    puts
  end
end

