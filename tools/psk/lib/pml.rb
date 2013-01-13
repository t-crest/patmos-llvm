#
# PML data format classes
# Provide smart accessors, caching, etc.
module PML

  # TODO create label depending on architecture?
  RE_HEX=/[0-9A-Fa-f]/

  def parse_mbb_label(label)
    label =~ /\A\.LBB(\d+)_(\d+)$/
    [$1.to_i, $2.to_i] if $1
  end

  def merge_ranges(r1,r2=nil)
    die "merge_ranges: first argument is nil" unless r1
    r1=Range.new(r1,r1) unless r1.kind_of?(Range)
    return r1 unless r2
    [r1.min,r2.min].min .. [r1.max,r2.max].max
  end

  class Adapter
    attr_reader :data
    def to_yaml
      @data.to_yaml
    end
  end

  # Smart Adapter for read-only lists
  class ListAdapter
    attr_reader :list
    def initialize(list)
      @list = list
      build_lookup
    end
    def get(key)
      v = @named[key]
      raise Exception.new("#{self.class}.get: No such object: #{key} for #{self}") unless v
      v
    end
    def to_s
      list.to_s
    end
    def by_address(address)
      v = @address[address]
      raise Exception.new("No object with such address:  #{address}") unless v
      v
    end
    def originated_from(src)
      build_rev_map unless @revmap
      if @dups[src]
        raise Exception.new("More than one machine function map to #{src}")
      elsif ! @revmap[src]
        raise Exception.new("No object originated from #{src}")
      else
        @revmap[src]
      end
    end
    # we could use a delegator for the next 2 methods
    def first
      @list.first
    end
    def each
      @list.each { |v| yield v }
    end
    private
    def build_lookup
      @named,@address = {}, {}
      @list.each do |v|
        @named[v.name] = v
        @address[v.address] = v
      end
    end
    def build_rev_map
      @revmap = {}
      @dups = {}
      @list.each do |v|
        if src = v['mapsto']
          if @revmap[src]
            @dups[src] = true
          elsif ! @dups[src]
            @revmap[src] = v
          end
        end
      end
    end
  end

  # Smart adapter for lists where the name is the index in the list
  class IndexedListAdapter < ListAdapter
    def initialize(list)
      super(list)
    end
    def [](index)
      @list[index]
    end
  end
  
  # class providing convenient accessors and additional program information derived
  # from PML files
  class PMLDoc < Adapter
    attr_reader :bitcode_functions,:machine_functions,:flowfacts

    def initialize(data_or_io)
      stream = if data_or_io.kind_of?(Array)
                 data_or_io
               elsif data_or_io.kind_of?(IO)
                 stream = YAML::load_stream(data_or_io)
                 stream = stream.documents if stream.respond_to?(:documents) # ruby 1.8 compat
                 stream
               elsif
                 [data_or_io]
               end
      if stream.length == 1
        @data = stream[0]
      else
        @data = PMLDoc.merge_stream(stream)
      end
      @bitcode_functions = ListAdapter.new(@data['bitcode-functions'].map { |f| Function.new(f) })
      @machine_functions = ListAdapter.new(@data['machine-functions'].map { |f| Function.new(f) })
    end
    # FIXME: use list adapter and add_flowfact
    def flowfacts
      @data['flowfacts'] ||= []
    end
    def add_flowfact(flowfact)
      @data['flowfacts'] ||= []
      @data['flowfacts'].push(flowfact.data)
    end
    def add_timing(timing_entry)
      @data['timing'] ||= []
      @data['timing'].push(timing_entry.data)      
    end
    def to_s
      "PMLDoc{bitcode-functions: |#{bitcode_functions.length}|, machine-functions: |#{machine_functions.length}"+
        ", flowfacts: |#{flowfacts.length}|}"
    end
    def dump_to_file(filename)
      if filename.nil?
        dump($>)
      else
        File.open(filename, "w") do |fh|
          dump(fh)
        end
      end
    end
    def dump(io)
      io.write(YAML::dump(data))
    end

    def PMLDoc.from_file(filename)
      File.open(filename) { |fh| PMLDoc.new(fh) }
    end

    def PMLDoc.merge_stream(stream)
      merged_doc = {}
      stream.each do |doc|
        doc.each do |k,v|
          if(v.kind_of? Array)
            (merged_doc[k]||=[]).concat(v)
          elsif(! merged_doc[k])
            merged_doc[k] = doc[k]
          elsif(merged_doc[k] != doc[k])
            raise Exception.new("psk-merge: mismatch in non-list attribute #{k}: #{merged_doc[k]} and #{doc[k]}")
          end
        end
      end
      merged_doc
    end
  end

  # Qualified name for functions
  class FunctionRef < Adapter
    attr_reader :qname
    def initialize(name)
      @qname = name
      @data = { 'function' => name }
    end
  end

  # Qualified name for blocks
  class BlockRef < Adapter
    attr_reader :qname
    def initialize(fname,bname)
      @qname = Block.get_label(fname, bname)
      @data = { 'function' => fname, 'block' => bname }
    end
  end

  # Qualified name for instructions
  class InstructionRef < Adapter
    attr_reader :qname
    def initialize(fname,bname,index)
      @qname = "#{Block.get_label(fname,bname)}_#{index}"
      @data = { 'function' => fname, 'block' => bname, 'instruction' => index }
    end
  end

  # References to Program Points (functions, blocks, instructions)
  class ProgramPointAdapter < Adapter
    attr_reader :name, :qname, :ref
    def address
      @data['address']
    end
    def qname
      ref.qname
    end
    def address=(value)
      @data['address']=value
    end
    def ==(other)
      qname == other.qname
    end
    def <=>(other)
      qname <=> other.qname
    end
    def eql?(other); self == other ; end
  end

  #  PML function wrapper
  class Function < ProgramPointAdapter
    attr_reader :blocks, :loops
    def initialize(mf)
      @data = mf
      @name = @data['name']
      @ref = FunctionRef.new(name)
      @hash = name.hash
      @loops = []
      @blocks = ListAdapter.new(@data['blocks'].map { |mbb| Block.new(self, mbb) })
      blocks.each do |block|
        if(block.loopheader?)
          @loops.push(block)
        end
      end
    end
    def [](k)
      internal_error "Function: do not access blocks directly" if k=='blocks'
      internal_error "Function: do not access loops directly" if  k=='loops'
      @data[k]
    end
    def hash; @hash ; end
    def to_s
      "#{@data['mapsto']}/#{name}"
    end
    def address
      @data['address'] || blocks.first.address
    end
  end

  # Class representing PML Basic Blocks
  class Block < ProgramPointAdapter
    attr_reader :function,:instructions,:loopnest
    def initialize(function,mbb)
      @data = mbb
      @function = function
      @name = @data['name']
      @ref = BlockRef.new(function.name, name)
      @hash = qname.hash
      @is_loopheader = @data['loops'] && @data['loops'].first == self.name
      @loopnest = (@data['loops']||[]).length
      @instructions = IndexedListAdapter.new(@data['instructions'].map { |ins| 
                                               Instruction.new(self, ins) 
                                             })
    end
    def [](k)
      internal_error "Block: do not access instructions directly" if k=='instructions'
      @data[k]
    end
    def to_s
      "#{function['mapsto']}/#{qname}"
    end
    def loopname
      { 'function' => function.name, 'loop' => name }
    end
    def hash; @hash ; end
    def loopheader? ; @is_loopheader ; end
    def calls?
      instructions.list.any? { |i| (i['callees']||[]).length > 0 }
    end
    def loopref
      { 'function' => function.name, 'loop' => name }
    end
    # LLVM specific (arch specific?)
    def label
      qname
    end
    def Block.get_label(fname,bname)
      die "Bad arguments to get_label:#{fname.class}_#{bname.class}" unless [fname,bname].all?{ |s|
        s.kind_of?(String) || s.kind_of?(Integer)
      }
      ".LBB#{fname}_#{bname}"
    end
  end

  # Smart reference to a PML instruction
  class Instruction < ProgramPointAdapter
    attr_reader :data, :block
    def initialize(block,ins)
      @block = block
      @data = ins
      @name = index
      @ref  = InstructionRef.new(function.name,block.name,name)
      @hash = @qname.hash
    end
    def function
      block.function
    end
    def [](k)
      @data[k]
    end
    def to_s
      "#{function['mapsto']}/#{qname}"
    end
    def hash; @hash ; end
    def index ; @data['index']; end
  end

  # Flow Fact utility class
  class FlowFact < Adapter
    def initialize(data)
      @data = data
    end
    def []=(k,v)
      @data[k] = v
    end
    def add_term(ppref,factor=1)
      @data['lhs'] ||= []
      @data['lhs'].push({ 'factor' => factor, 'program-point' => ppref.data })
      self
    end
    def FlowFact.block_frequency(scope, block, freq, fact_context, classification)
      flowfact = FlowFact.new(fact_context.dup)
      flowfact['scope'] = scope.data
      flowfact['op'] = 'less-equal'
      flowfact['rhs'] = freq.max
      flowfact['classification'] = 'block-global'
      flowfact.add_term(block.ref)
    end
    def FlowFact.calltargets(scope, cs, receiverset, fact_context, classification)
      flowfact = FlowFact.new(fact_context.dup)
      flowfact['scope'] = scope.data
      flowfact['op'] = 'equal'
      flowfact['rhs'] = 0
      flowfact['classification'] = classification
      flowfact.add_term(cs.ref, -1)
      receiverset.each do |function| 
        flowfact.add_term(function.ref, 1)
      end
      flowfact
    end
    def FlowFact.loop_bound(loop, freq, fact_context, classification)
      flowfact = FlowFact.new(fact_context.dup)
      flowfact['scope'] = loop.loopref
      flowfact['op'] = 'less-equal'
      flowfact['rhs'] = freq.max
      flowfact['classification'] = classification
      flowfact.add_term(loop.ref)
      flowfact
    end
    def FlowFact.infeasible(scope, block, fact_context, classification) 
      flowfact = FlowFact.new(fact_context.dup)
      flowfact['scope'] = scope.data
      flowfact['op'] = 'equal'
      flowfact['rhs'] = 0
      flowfact['classification'] = classification
      flowfact.add_term(block.ref)
      flowfact
    end
  end

  # timing entries are used to record WCET analysis results or measurement results
  class TimingEntry < Adapter
    def initialize(scope, cycles, context)
      @data = context.dup
      @data['scope'] ||= scope.data
      @data['cycles'] = cycles
    end
    def to_s
      @data.to_s
    end
  end
end
