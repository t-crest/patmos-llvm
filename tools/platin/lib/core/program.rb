#
# platin tool set
#
# core library for program representation
#
require 'core/utils'
require 'core/pmlbase'

module PML

  class Reference < PMLObject
    include QNameObject
    def Reference.from_pml(functions, data)
      fname = data['function']
      bname = data['block']
      lname = data['loop']
      iname = data['instruction']
      is_edge = ! data['edgesource'].nil?

      # support for compact notation
      return InstructionRef.from_qname(functions,iname) if iname && ! bname
      return LoopRef.from_qname(functions,data['loop']) if lname && ! fname
      return BlockRef.from_qname(functions,bname)       if bname && ! fname
      assert("PML Reference: no function attribute") { fname }
      function = functions.by_name(fname)

      if (lname || bname)
        block = function.blocks.by_name(lname || bname)
        if iname
          instruction = block.instructions[iname]
          return InstructionRef.new(instruction,data)
        elsif lname
          return LoopRef.new(block,data)
        else
          return BlockRef.new(block,data)
        end
      elsif is_edge
        src = data['edgesource']
        bb_src = function.blocks.by_name(src)
        bb_dst = function.blocks.by_name(data['edgetarget']) if data['edgetarget']
        return EdgeRef.new(bb_src, bb_dst, data)
      else
        return FunctionRef.new(function, data)
      end
    end
  end

  # Qualified name for functions
  class FunctionRef < Reference
    attr_reader :function
    def initialize(function, data = nil)
      @function = function
      @qname = function.qname
      set_yaml_repr(data)
    end
    def to_s
      "#<FunctionRef: #{function}>"
    end
    def to_pml
      { 'function' => @function.name }
    end
  end

  # Qualified name for blocks
  class BlockRef < Reference
    attr_reader :function, :block, :qname
    def initialize(block, data = nil)
      @block = block
      @function = block.function
      @qname = block.qname
      set_yaml_repr(data)
    end
    def to_s
      "#<BlockRef: #{@block.qname}"
    end
    def to_pml
      { 'function' => function.name, 'block' => block.name }
    end
    def BlockRef.from_qname(functions,qn)
      fn,bn = qn.split('/',2).map { |n| YAML::load(n) }
      functions.by_name(fn).blocks.by_name(bn).ref
    end
  end

  # Qualified name for loops
  class LoopRef < Reference
    attr_reader :function, :loopblock, :qname
    def initialize(block, data = nil)
      die("Block#loopref: #{block.qname} is not a loop header") unless block.loopheader?
      @loopblock = block
      @function = block.function
      @qname = block.qname
      set_yaml_repr(data)
    end
    def to_s
      "#<LoopRef: #{loopblock.qname}>"
    end
    def to_pml
      { 'function' => loopblock.function.name, 'loop' => loopblock.name }
    end
    def LoopRef.from_qname(functions,qn)
      fn,bn = qn.split('/',2).map { |n| YAML::load(n) }
      LoopRef.new(functions.by_name(fn).blocks.by_name(bn))
    end
  end

  class EdgeRef < Reference
    attr_reader :source, :target
    def initialize(source, target, data = nil)
      assert("PML EdgeRef: source and target need to be blocks, not #{source.class}/#{target.class}") {
        source.kind_of?(Block) && (target.nil? || target.kind_of?(Block))
      }
      assert("PML EdgeRef: source and target function need to match") { target.nil? || source.function == target.function }

      @source, @target = source, target
      @name = "#{source.name}->#{target ? target.name : '' }"
      @qname = "#{source.qname}->#{target ? target.name : '' }"
      set_yaml_repr(data)
    end
    def ref
      self
    end
    def exitedge?
      target.nil?
    end
    def function
      source.function
    end
    def to_s
      "#{source.to_s}->#{target ? target.qname : 'exit'}"
    end
    def to_pml
      pml = { 'function' => source.function.name,
        'edgesource' => source.name }
      pml['edgetarget'] = target.name if target
      pml
    end
  end

  # Qualified name for instructions
  class InstructionRef < Reference
    attr_reader :function, :block, :instruction
    def initialize(instruction, data = nil)
      @instruction = instruction
      @block, @function = instruction.block, instruction.function
      @qname = instruction.qname
      set_yaml_repr(data)
    end
    def to_s
      "#<InstructionRef: #{@instruction.qname}>"
    end
    def to_pml
      { 'function' => function.name, 'block' => block.name, 'instruction' => instruction.name }
    end
    def InstructionRef.from_qname(functions,qn)
      fn,bn,iname = qn.split('/',3).map { |n| YAML::load(n) }
      functions.by_name(fn).blocks.by_name(bn).instructions[iname].ref
    end
  end

  # List of functions in the program
  class FunctionList < PMLList
    extend PMLListGen
    pml_name_index_list(:Function,[:address,:label],[])

    # customized constructor
    def initialize(data, opts)
       @list = data.map { |f| Function.new(f, opts) }
       set_yaml_repr(data)
       build_index
    end

    # return [rs, unresolved]
    # rs .. list of (known functions) reachable from name
    # unresolved .. set of callsites that could not be resolved
    def reachable_from(name)
      unresolved = Set.new
      rs = reachable_set(by_name(name)) { |f|
        callees = []
        f.callsites.each { |cs|
          cs.callees.each { |n|
            if(f = by_label(n,false))
              callees.push(f)
            elsif(f = by_name(n,false))
              callees.push(f)
            else
              unresolved.add(cs)
            end
          }
        }
        callees
      }
      [rs, unresolved]
    end

    def instruction_by_address(addr)
      if ! @instruction_by_address
        @instruction_by_address = {}
        self.each { |f| f.instructions.each { |i| @instruction_by_address[i.address] = i } }
      end
      @instruction_by_address[addr]
    end
  end

  # List of PML basic blocks in a function
  class BlockList < PMLList
    extend PMLListGen
    pml_name_index_list(:Block)
    # customized constructor
    def initialize(function, data)
       @list = data.map { |b| Block.new(function, b) }
       @list.each_with_index { |block,ix| block.layout_successor=@list[ix+1] }
       set_yaml_repr(data)
       build_index
    end
  end

  # List of PML instructions in a block
  class InstructionList < PMLList
    extend PMLListGen
    pml_name_index_list(:Instruction)
    # customized constructor
    def initialize(block, data)
      @list = data.map { |i| Instruction.new(block, i) }
      set_yaml_repr(data)
    end
    def [](index)
      @list[index]
    end
  end

  # References to Program Points (functions, blocks, instructions)
  class ProgramPointProxy < PMLObject
    include QNameObject
    attr_reader :name
    def address ; data['address'] ; end
    def address=(addr); data['address'] = addr; end
  end

  # PML call graph
  class Callgraph < PMLObject
  end

  # PML function arguments
  class ArgumentList < PMLList
    extend PMLListGen
    pml_list(:FunctionArgument,[:name])
    # customized constructor
    def initialize(function, data)
      @list = data.map { |a| FunctionArgument.new(function, a) }
      set_yaml_repr(data)
      build_index
    end
  end

  class FunctionArgument < PMLObject
    def initialize(function, data)
      set_yaml_repr(data)
      @function = function
    end
    def name
      data['name']
    end
    def index
      data['index']
    end
    def maps_to_register?
      registers.length == 1
    end
    def registers
      data['registers']
    end
  end

  # PML function wrapper
  class Function < ProgramPointProxy
    attr_reader :blocks, :loops, :arguments
    def initialize(data, opts)
      set_yaml_repr(data)
      @name = data['name']
      @qname = name
      @loops = []
      @labelkey = opts[:labelkey]
      @blocks = BlockList.new(self, data['blocks'])
      @arguments = ArgumentList.new(self, data['arguments'] || [])
      @blocks.each do |block|
        if(block.loopheader?)
          @loops.push(block)
        end
      end
    end
    def ref
      FunctionRef.new(self)
    end
    def [](k)
      assert("Function: do not access blocks/loops directly") { k!='blocks'&&k!='loops'}
      data[k]
    end
    def mapsto
      data['mapsto']
    end
    def to_s
      s = name
      s = "(#{data['mapsto']})#{s}" if data['mapsto']
      s
    end
    def entry_block
      blocks.first
    end
    def address
      data['address'] || blocks.first.address
    end
    def label
      data[@labelkey] || blocks.first.label
    end

    def instructions
      blocks.inject([]) { |insns,b| insns.concat(b.instructions.list) }
    end

    # all (intra-procedural) edges in this function
    def edges
      Enumerator.new do |ss|
        blocks.each { |b|
          b.outgoing_edges.each { |e|
            ss << e
          }
        }
      end
    end

    # all callsites found in this function
    def callsites
      Enumerator.new do |ss|
        blocks.each { |b|
          b.callsites.each { |cs|
            ss << cs
          }
        }
      end
    end
  end # of class Function

  # Class representing PML Basic Blocks
  class Block < ProgramPointProxy
    attr_reader :function,:instructions,:loopnest
    def initialize(function,data)
      set_yaml_repr(data)
      @function = function
      @name = data['name']
      @qname = "#{function.name}/#{@name}"

      loopnames = data['loops'] || []
      @loopnest = loopnames.length
      @is_loopheader = loopnames.first == self.name
      @instructions = InstructionList.new(self, data['instructions'] || [])
    end
    def mapsto
      data['mapsto']
    end
    # loops (not ready at initialization time)
    def loops
      return @loops if @loops
      @loops = (data['loops']||[]).map { |l| function.blocks.by_name(l) }
    end

    # returns true if a CFG edge from the given source node to this block is a back edge
    def backedge_target?(source)
      return false unless loopheader?
      # if the loopnest of the source is smaller than ours, it is certainly not in the same loop
      return false unless source.loopnest >= loopnest
      # if the source is in the same loop, our loops are a suffix of theirs
      # as loop nests form a tree, the suffices are equal if there first element is
      source_loop_index = source.loopnest - loopnest
      source.loops[source_loop_index] == self
    end

    # returns true if a CFG edge from this block to the given target is an exit edge
    def exitedge_source?(target)
      if target.loopnest > loopnest
        false
      elsif target.loopnest < loopnest
        true
      else
        loops[0] != target.loops[0]
      end
    end

    # return true if the block does not contain any actual instructions (labels are ok)
    # FIXME: blocks are currently also considered to be empty if they only contain inline asm
    def empty?
      instructions.empty? || instructions.all? { |i| i.size == 0 }
    end

    # block predecessors (not ready at initialization time)
    def predecessors
      return @predecessors if @predecessors
      @predecessors = (data['predecessors']||[]).map { |s| function.blocks.by_name(s) }.uniq.freeze
    end

    # block successors (not ready at initialization time)
    def successors
      return @successors if @successors
      @successors = (data['successors']||[]).map { |s| function.blocks.by_name(s) }.uniq.freeze
    end

    # edge to the given target block (reference)
    def edge_to(target)
      EdgeRef.new(self, target)
    end

    # edge to the function exit
    def edge_to_exit
      EdgeRef.new(self, nil)
    end

    # yields outgoing edges (references)
    def outgoing_edges
      Enumerator.new do |ss|
        successors.each { |s|
          ss << edge_to(s)
        }
        ss << edge_to(nil) if self.may_return?
      end
    end

    # set the block directly succeeding this one in the binary layout
    def layout_successor=(block)
      @layout_successor=block
    end

    # return a successor which is (might) be reached via fallthrough
    # NOTE: this is a heuristic at the moment
    def fallthrough_successor
      if successors.include?(@layout_successor)
        @layout_successor
      else
        nil
      end
    end

    # the unique successor, if there is one
    def next
      (successors.length == 1) ? successors.first : nil
    end

    # true if this is a loop header
    def loopheader?
      @is_loopheader
    end

    # true if this is the header of a loop that has a preheader
    def has_preheader?
      return @has_preheader unless @has_preheader.nil?
      return (@has_preheader = false) unless loopheader?
      preheaders = []
      predecessors.each { |pred|
        next if self.backedge_target?(pred)
        preheaders.push(pred)
      }
      @has_preheader = (preheaders.length == 1)
    end

    # true if this block may return from the function
    def may_return?
      @returnsites = instructions.list.select { |i| i.returns? } unless @returnsites
      ! @returnsites.empty? || must_return?
    end

    def must_return?
      successors.empty?
    end

    # whether this block has a call instruction
    def calls?
      ! callsites.empty?
    end

    # list of callsites in this block
    def callsites
      return @callsites if @callsites
      @callsites = instructions.list.select { |i| i.callees.length > 0 }
    end

    # XXX: LLVM specific/arch specific
    def label
      Block.get_label(function.name, name)
    end

    # XXX: LLVM specific/arch specific
    def Block.get_label(fname,bname)
      ".LBB#{fname}_#{bname}"
    end

    # reference to this block
    def ref
      BlockRef.new(self)
    end

    # reference to the loop represented by the block (needs to be the header of a reducible loop)
    def loopref
      LoopRef.new(self)
    end

    # string representation
    def to_s
      if function.mapsto
        "(#{function.mapsto})#{qname}"
      else
        qname
      end
    end
  end

  # Proxy for PML instructions
  class Instruction < ProgramPointProxy
    attr_reader :block
    def initialize(block,data)
      set_yaml_repr(data)
      @block = block
      @name = index
      @qname = "#{block.qname}/#{@name}"
    end

    def index
      data['index']
    end
    def ref
      InstructionRef.new(self)
    end

    # type of branch this instruction realizes (if any)
    def branch_type
      data['branch-type']
    end

    # whether this instruction includes a call
    def calls?
      ! callees.empty?
    end

    # calless of this instruction
    def callees
      data['callees'] || []
    end

    # whether this instruction is an indirect (unresolved) call
    def unresolved_call?
      callees.include?("__any__")
    end

    # whether this instruction isa branch
    def branches?
      ! branch_targets.empty?
    end

    # branch targets
    def branch_targets
      return @branch_targets if @branch_targets
      @branch_targets = (data['branch-targets']||[]).map { |s| function.blocks.by_name(s) }.uniq.freeze
    end

    # whether this instruction returns
    def returns?
      branch_type == 'return'
    end

    # number of delay slots, if this is a branch instruction
    def delay_slots
      data['branch-delay-slots'] || 0
    end

    def sc_fill
      data['stack-cache-fill']
    end

    # whether the given block is still a successor if we are at this instruction in the current block
    def live_successor?(target)
      ix = index
      while i = block.instructions[ix]
        return true if i.branch_targets.include?(target)
        ix+=1
      end
      return true if block.fallthrough_successor == target
      return false
    end

    # the function corresponding the instruction is contained in
    def function
      block.function
    end
    # the next instruction in the instruction list, or the first instruction of the only successor block
    def next
      block.instructions[index+1] || (block.next ? block.next.instructions.first : nil)
    end

    # size of this instruction (binary level)
    def size   ; data['size'] ; end

    def opcode ; data['opcode'] ; end

    def to_s
      s = qname
      s = "(#{function.mapsto})#{s}" if function.mapsto
      s
    end
  end

  # List of relation graphs (unmodifiable)
  class RelationGraphList < PMLList

    # non-standard pml list
    #
    def initialize(data, srclist, dstlist)
      @list = data.map { |rgdata| RelationGraph.new(rgdata, srclist, dstlist) }
      set_yaml_repr(data)
      build_lookup
    end

    # whether there is a relation graph involving function
    # @name@ on level @level@
    #
    def has_named?(name, level)
      ! @named[level][name].nil?
    end

    # get relation graph by function's name on the specified level
    #
    def by_name(name, level)
      assert("RelationGraphList#by_name: level != :src,:dst") { [:src,:dst].include?(level) }
      lookup(@named[level], name, "#{level}-name", false)
    end

    def build_lookup
      @named = { :src => {}, :dst => {} }
      @list.each do |rg|
        add_lookup(@named[:src], rg.src.name, rg, "src-name")
        add_lookup(@named[:dst], rg.dst.name, rg, "dst-name")
      end
    end
  end

  # List of relation graph nodes (unmodifiable)
  class RelationNodeList < PMLList
    extend PMLListGen
    pml_name_index_list(:RelationNode)

    def initialize(rg, data)
      @list = data.map { |n| RelationNode.new(rg, n) }
      set_yaml_repr(data)
      build_index
      build_relation_index
    end

    # get relation graph node(s) that reference the specified basic block
    #
    def by_basic_block(bb, level)
      assert("RelationNodeList#by_basic_block: level != :src,:dst") { [:src,:dst].include?(level) }
      lookup(@basic_block_index[level], bb, "#{level}-block", false) || []
    end

private
    def build_relation_index
      @basic_block_index = { :src => {}, :dst => {} }
      @list.each do |rgn|
        [:src,:dst].each do |level|
          bb = rgn.get_block(level)
          next unless bb
          (@basic_block_index[level][bb] ||= []).push(rgn)
        end
      end
    end
  end

  # Relation Graphs
  class RelationGraph < PMLObject
    attr_reader :src_functions, :dst_functions, :src, :dst, :nodes
    def initialize(data,src_funs,dst_funs)
      set_yaml_repr(data)
      @src_functions, @dst_functions = src_funs, dst_funs
      @src = src_funs.by_name(data['src']['function'])
      @dst = dst_funs.by_name(data['dst']['function'])
      @nodes = RelationNodeList.new(self, data['nodes'])
    end
    def get_function(level)
      level == :src ? @src : @dst
    end
    def qname
      "#{src.qname}<>#{dst.qname}"
    end
  end

  # Relation Graph node
  class RelationNode < PMLObject
    include QNameObject
    attr_reader :name, :rg
    def initialize(rg, data)
      set_yaml_repr(data)
      @rg = rg
      @name = data['name']
      @qname = "#{@rg.qname}_#{@name}"
      @successors = {} # lazy initialization
    end

    # get basic block for the specified level
    # :progress and :entry provide both blocks, :src and :dst
    # blocks on the respective level, and :exit no block
    #
    def get_block(level)
      return nil unless data["#{level}-block"]
      rg.get_function(level).blocks.by_name(data["#{level}-block"])
    end

    # returns one out of [ :progress, :dst, :src, :entry, :exit ]
    #
    def type
      data['type'].to_sym
    end

    # true if this is a :dst or :src node
    #
    def unmapped?
      type == :src || type == :dst
    end

    def successors_matching(block, level)
      assert("successors_matching: nil argument") { ! block.nil? }
      successors(level).select { |b|
        succblock = b.get_block(level)
        ! succblock.nil? && succblock == block
      }
    end
    def successors(level)
      return @successors[level] if @successors[level]
      @successors[level] = (data["#{level}-successors"]||[]).map { |succ|
        @rg.nodes.by_name(succ)
      }.uniq
      @successors[level]
    end
    def to_s
      "#{type}:#{qname}"
    end
  end

end # module PML
