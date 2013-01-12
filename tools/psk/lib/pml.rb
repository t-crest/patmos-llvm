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
      raise Exception.new("No such object: #{key}") unless v
      v
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

    def flowfacts
      @data['flowfacts'] ||= []
      @data['flowfacts']
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

  # References to Program Points (functions, blocks, instructions)
  class ProgramPointAdapter < Adapter
    attr_reader :name, :qname
    def address
      @data['address']
    end
    def address=(value)
      @data['address']=value
    end
  end

  # Smart Reference to a PML function
  class Function < ProgramPointAdapter
    attr_reader :blocks, :loops
    def initialize(mf)
      @data = mf
      @name = @data['name']
      @qname = @name
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
      die "Function: do not access blocks directly" if k=='blocks'
      die "Function: do not access loops directly" if  k=='loops'
      @data[k]
    end
    def to_s
      "#{@data['mapsto']}/#{name}"
    end
    def ==(other)
      return false unless other.kind_of?(Function)
      name == other.name
    end
    def hash; @hash ; end
    def eql?(other); self == other ; end
    def address
      blocks.first.address
    end
  end

  # Smart reference to a PML machine basic block
  class Block < ProgramPointAdapter
    attr_reader :function,:instructions,:loopnest
    def initialize(function,mbb)
      @data = mbb
      @function = function
      @name = @data['name']
      @qname = label
      @hash = @qname.hash
      @is_loopheader = @data['loops'] && @data['loops'].first == self.name
      @loopnest = (@data['loops']||[]).length
      @instructions = IndexedListAdapter.new(@data['instructions'].map { |ins| 
                                               Instruction.new(self, ins) 
                                             })
    end
    def [](k)
      die "Block: do not access instructions directly" if k=='instructions'
      @data[k]
    end
    def to_s
      "#{function['mapsto']}/#{qname}"
    end
    def ==(other)
      return false unless other.kind_of?(Block)
      qname == other.qname
    end
    def hash; @hash ; end
    def eql?(other); self == other ; end
    def loopheader? ; @is_loopheader ; end
    def calls?
      instructions.list.any? { |i| (i['callees']||[]).length > 0 }
    end
    # LLVM specific (arch specific?)
    def label
      Block.get_label(function['name'],name)
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
      @qname = "#{@block.qname}_#{ins['index']}"
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
    def ==(other)
      return false unless other.kind_of?(Block)
      qname == other.qname
    end
    def hash; @hash ; end
    def eql?(other); self == other ; end
    def index ; @data['index']; end
  end

  # Flow Fact utility class
  class FlowFact
    attr_reader :data
    def initialize(initdata)
      @data = initdata.dup
      @data['lhs'] ||= []
    end
    def []=(k,v)
      @data[k] = v
    end
    def add_term(pp,factor=1)
      @data['lhs'].push({ 'factor' => factor, 'program-point' => pp })
      self
    end
    def FlowFact.block_frequency(scope, block, freq, fact_context, classification)
      flowfact = FlowFact.new(fact_context)
      flowfact['scope'] = scope
      flowfact['op'] = 'less-equal'
      flowfact['rhs'] = freq.max
      flowfact['classification'] = 'block-global'
      flowfact.add_term(FlowFact.block(block))
    end
    def FlowFact.calltargets(scope, cs, receiverset, fact_context, classification)
      flowfact = FlowFact.new(fact_context)
      flowfact['scope'] = scope
      flowfact['op'] = 'equal'
      flowfact['rhs'] = 0
      flowfact['classification'] = classification
      flowfact.add_term(FlowFact.instruction(cs), -1)
      receiverset.each do |function| 
        flowfact.add_term(FlowFact.function(function), 1)
      end
      flowfact
    end
    def FlowFact.loop_bound(loop, freq, fact_context, classification)
      flowfact = FlowFact.new(fact_context)
      flowfact['scope'] = FlowFact.loop(loop)
      flowfact['op'] = 'less-equal'
      flowfact['rhs'] = freq.max
      flowfact['classification'] = classification
      flowfact.add_term(FlowFact.block(loop))
      flowfact
    end
    def FlowFact.infeasible(scope, block, fact_context, classification) 
      flowfact = FlowFact.new(fact_context)
      flowfact['scope'] = scope
      flowfact['op'] = 'equal'
      flowfact['rhs'] = 0
      flowfact['classification'] = classification
      flowfact.add_term(FlowFact.block(block))
      flowfact
    end
    def FlowFact.function(fref)
      { 'function' => fref.name }
    end
    def FlowFact.block(blockref)
      { 'function' => blockref.function.name, 'block' => blockref.name }
    end
    def FlowFact.loop(blockref)
      { 'function' => blockref.function.name, 'loop' => blockref.name }
    end
    def FlowFact.instruction(insref)
      { 'instruction' => insref.name }.merge(FlowFact.block(insref.block))
    end
  end
end
