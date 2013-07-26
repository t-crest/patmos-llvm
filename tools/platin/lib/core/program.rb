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
      is_edge = ! data['edgetarget'].nil?

      # support for compact notation
      return InstructionRef.from_qname(functions,iname) if iname && ! bname
      return LoopRef.from_qname(functions,data['loop']) if lname && ! fname
      return BlockRef.from_qname(functions,bname)       if bname && ! fname
      assert("PML Reference: no function attribute") { fname }
      function = functions.by_name(fname)

      if bname
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
        bb_dst = function.blocks.by_name(data['edgetarget'])
        return EdgeRef.new(bb_src, bb_dst, data)
      else
        return FunctionRef.new(function, data)
      end
    end
    def ==(other)
      return false if ! other.kind_of?(Reference)
      return qname == other.qname
    end
  end

  # Qualified name for functions
  class FunctionRef < Reference
    attr_reader :function
    def initialize(function, data = nil)
      @function = function
      @qname = function.qname
      set_data(data)
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
      set_data(data)
    end
    def to_s
      "#<BlockRef: #{@block.qname}"
    end
    def to_pml
      { 'block' => block.qname }
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
      @loopblock = block
      @function = block.function
      @qname = block.qname
      set_data(data)
    end
    def to_s
      "#<LoopRef: #{loopblock.qname}"
    end
    def to_pml
      { 'loop' => loopblock.qname }
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
        source.kind_of?(Block) && target.kind_of?(Block)
      }
      assert("PML EdgeRef: source and target function need to match") { source.function == target.function }

      @source, @target = source, target
      @name = "#{source.name}->#{target.name}"
      @qname = "#{source.qname}->#{target.name}"
      set_data(data)
    end
    def function
      source.function
    end
    def to_s
      qname
    end
    def to_pml
      { 'function' => source.function.name,
        'edgesource' => source.name,
        'edgetarget' => target.name }
    end
  end

  # Qualified name for instructions
  class InstructionRef < Reference
    attr_reader :function, :block, :instruction
    def initialize(instruction, data = nil)
      @instruction = instruction
      @block, @function = instruction.block, instruction.function
      @qname = instruction.qname
      set_data(data)
    end
    def block
      instruction.block
    end
    def to_s
      "#<InstructionRef: #{@instruction.qname}>"
    end
    def to_pml
      { 'instruction' => instruction.qname }
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
       set_data(data)
       build_index
    end

    # return [rs, unresolved]
    # rs .. list of (known functions) reachable from name
    # unresolved .. set of callsites that could not be resolved
    def reachable_from(name)
      unresolved = Set.new
      rs = reachable_set(by_name(name)) { |f|
        callees = []
        f.each_callsite { |cs|
          cs.callees.each { |n|
            if(f = @labelled[n])
              callees.push(f)
            elsif(f = @named[n])
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
       set_data(data)
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
      set_data(data)
    end
    def [](index)
      @list[index]
    end
  end

  # References to Program Points (functions, blocks, instructions)
  class ProgramPointProxy < PMLObject
    include QNameObject
    attr_reader :name
    def address
      data['address']
    end
    def address=(value)
      data['address']=value
    end
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
     set_data(data)
    end
  end

  class FunctionArgument < PMLObject
    def initialize(function, data)
      set_data(data)
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
    attr_reader :blocks, :loops
    def initialize(data, opts)
      set_data(data)
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
      blocks.inject([]) { |insns,b| insns.concat(b.instructions) }
    end
    def each_callsite
      blocks.each do |block|
        block.callsites.each do |cs|
          yield cs
        end
      end
    end
  # end of class Function
  end


  # Class representing PML Basic Blocks
  class Block < ProgramPointProxy
    attr_reader :function,:instructions,:loopnest
    def initialize(function,data)
      set_data(data)
      @function = function
      @name = data['name']
      @qname = "#{function.name}/#{@name}"

      loopnames = data['loops'] || []
      @loopnest = loopnames.length
      @is_loopheader = loopnames.first == self.name
      @instructions = InstructionList.new(self, data['instructions'] || [])
    end
    def [](k)
      assert("Do not access instructions via []") { k != 'instructions' }
      assert("Do not access predecessors/successors directly") { k != 'predecessors' && k != 'successors' }
      assert("Do not access loops directly") { k != 'loops' }
      data[k]
    end
    # loops: not ready at initialization time
    def loops
      return @loops if @loops
      @loops = (data['loops']||[]).map { |l| function.blocks.by_name(l) }
    end
    # whether a CFG edge from the given source node is a back edge
    def backedge_target?(source)
      return false unless loopheader?
      return false unless source.loopnest >= loopnest
      source_loop_index = source.loopnest - loopnest
      source.loops[source_loop_index] == self
    end
    # return true if the block does not contain any actual instructions (labels are ok)
    # FIXME: blocks are currently also considered to be empty if they only contain inline asm
    def empty?
      instructions.empty? || instructions.all? { |i| i.size == 0 }
    end
    # block predecessors; not ready at initialization time
    def predecessors
      return @predecessors if @predecessors
      @predecessors = (data['predecessors']||[]).map { |s| function.blocks.by_name(s) }.uniq.freeze
    end
    # block successors; not ready at initialization time
    def successors
      return @successors if @successors
      @successors = (data['successors']||[]).map { |s| function.blocks.by_name(s) }.uniq.freeze
    end
    def layout_successor=(block)
      @layout_successor=block
    end
    # XXX: (possible over-approximating) heuristic
    def fallthrough_successor
      if successors.include?(@layout_successor)
        @fallthrough_block = @layout_successor
      else
        @fallthrough_block = nil
      end
    end
    def next
      (successors.length == 1) ? successors.first : nil
    end
    def ref
      BlockRef.new(self)
    end
    def to_s
      if function['mapsto']
        "(#{function['mapsto']})#{qname}"
      else
        qname
      end
    end
    def loopheader?
      @is_loopheader
    end
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
    def callsites
      instructions.list.select { |i| i.callees.length > 0 }
    end
    def calls?
      ! callsites.empty?
    end

    def loopref
      assert("Block#loopref: not a loop header") { self.loopheader? }
      LoopRef.new(self)
    end

    # XXX: LLVM specific/arch specific
    def label
      Block.get_label(function.name, name)
    end

    def Block.get_label(fname,bname)
      ".LBB#{fname}_#{bname}"
    end
  end

  # Proxy for PML instructions
  class Instruction < ProgramPointProxy
    attr_reader :block
    def initialize(block,data)
      set_data(data)
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
    def calls?
      ! callees.empty?
    end
    def callees
      data['callees'] || []
    end
    def unresolved_call?
      callees.include?("__any__")
    end
    def branches?
      ! branch_targets.empty?
    end
    def branch_targets
      data['branch-targets'] || []
    end
    def returns?
      data['branch-type'] == 'return'
    end
    def delay_slots
      data['branch-delay-slots'] || 0
    end
    def function
      block.function
    end
    def [](k)
      data[k]
    end
    def next
      block.instructions[index+1] || (block.next ? block.next.instructions.first : nil)
    end
    def to_s
      s = qname
      s = "(#{function['mapsto']})#{s}" if function['mapsto']
      s
    end
    def size   ; data['size'] ; end
    def opcode ; data['opcode'] ; end
  end

  # List of relation graphs (unmodifiable)
  class RelationGraphList < PMLList
    # non-standard pml list
    def initialize(data, srclist, dstlist)
      @list = data.map { |rgdata| RelationGraph.new(rgdata, srclist, dstlist) }
      set_data(data)
      build_lookup
    end
    def has_named?(name, level)
      ! @named[level][name].nil?
    end
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
      set_data(data)
    end
  end

  # Relation Graphs
  class RelationGraph < PMLObject
    attr_reader :src_functions, :dst_functions, :src, :dst, :nodes
    def initialize(data,src_funs,dst_funs)
      set_data(data)
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
      set_data(data)
      @rg = rg
      @name = data['name']
      @qname = "#{@rg.qname}_#{@name}"
      @successors = {} # lazy initialization
    end
    def get_block(level)
      return nil unless data["#{level}-block"]
      rg.get_function(level).blocks.by_name(data["#{level}-block"])
    end
    # returns one out of [ :progress, :dst, :src, :entry, :exit ]
    def type
      data['type'].to_sym
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
