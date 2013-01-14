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

  class Proxy
    attr_reader :data
    def to_yaml
      @data.to_yaml
    end
  end

  # Proxy for read-only lists
  class ListProxy
    attr_reader :list
    def to_s
      list.to_s
    end
    # we could define delegators for all accessors and tests
    def first
      @list.first
    end
    def each
      @list.each { |v| yield v }
    end
    def length
      @list.length
    end
  end

  # Smart Proxy for functions, blocks and instructions
  class ProgramPointList < ListProxy
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

  # Proxy for lists where the name is the index in the list
  class IndexedList < ProgramPointList
    def initialize(list)
      super(list)
    end
    def [](index)
      @list[index]
    end
  end
  
  # class providing convenient accessors and additional program information derived
  # from PML files
  class PMLDoc < Proxy
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
      @bitcode_functions = ProgramPointList.new(@data['bitcode-functions'].map { |f| Function.new(f) })
      @machine_functions = ProgramPointList.new(@data['machine-functions'].map { |f| Function.new(f) })
      @data['flowfacts'] ||= []
      @flowfacts = FlowFactList.new(@data['flowfacts'])
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
  class FunctionRef < Proxy
    attr_reader :qname
    def initialize(name)
      @qname = name
      @data = { 'function' => name }
    end
  end

  # Qualified name for blocks
  class BlockRef < Proxy
    attr_reader :qname
    def initialize(fname,bname)
      @qname = Block.get_label(fname, bname)
      @data = { 'function' => fname, 'block' => bname }
    end
  end

  # Qualified name for instructions
  class InstructionRef < Proxy
    attr_reader :qname
    def initialize(fname,bname,index)
      @qname = "#{Block.get_label(fname,bname)}_#{index}"
      @data = { 'function' => fname, 'block' => bname, 'instruction' => index }
    end
  end

  # References to Program Points (functions, blocks, instructions)
  class ProgramPointProxy < Proxy
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
  class Function < ProgramPointProxy
    attr_reader :blocks, :loops
    def initialize(mf)
      @data = mf
      @name = @data['name']
      @ref = FunctionRef.new(name)
      @hash = name.hash
      @loops = []
      @blocks = ProgramPointList.new(@data['blocks'].map { |mbb| Block.new(self, mbb) })
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
    def label
      @data['label'] || blocks.first.label
    end

    # map from called function to list of callsites
    # XXX ugly: expects a block which resolves callees
    # returns nil if there are unresolved indirect calls
    def callgraph_successors
      return @cg_succs if @cg_succs
      @cg_succs = {}
      blocks.each do |block|
        block.callsites.each do |cs|
          if cs['callees'].include?("__any__")
            return (@cg_succs = nil)
          end
         cs['callees'].each do |c|
            f = yield c
            (@cg_succs[f]||=[]).push(cs)
          end
        end
      end
      @cg_succs
    end
  # end of class Function
  end

  # Class representing PML Basic Blocks
  class Block < ProgramPointProxy
    attr_reader :function,:instructions,:loopnest
    def initialize(function,mbb)
      @data = mbb
      @function = function
      @name = @data['name']
      @ref = BlockRef.new(function.name, name)
      @hash = qname.hash
      @is_loopheader = @data['loops'] && @data['loops'].first == self.name
      @loopnest = (@data['loops']||[]).length
      die("No instructions in #{@name}") unless @data['instructions']
      @instructions = IndexedList.new(@data['instructions'].map { |ins| 
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
    def callsites
      instructions.list.select { |i| (i['callees']||[]).length > 0 }
    end
    def calls?
      ! callsites.empty?
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
  
  # Proxy for PML instructions
  class Instruction < ProgramPointProxy
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

  # List of flowfacts (modifiable)
  class FlowFactList < ListProxy
    def initialize(data)
      @data = data
      @list = data.map { |ff| FlowFact.new(ff) }
      build_index
    end
    def add(ff)
      @list.push(ff)
      @data.push(ff.data)
      add_index(ff)
    end
    private
    def build_index
      @by_class = {}
      @list.each { |ff| add_index(ff) }
    end
    def add_index(ff)
      (@by_class[ff.classification]||=[]).push(ff)
    end
  end

  # Flow Fact utility class
  class FlowFact < Proxy
    def initialize(data)
      @data = data
    end
    def classification
      @data['classification']
    end
    def [](k)
      @data[k]
    end
    def []=(k,v)
      @data[k] = v
    end
    def add_term(ppref,factor=1)
      @data['lhs'] ||= []
      @data['lhs'].push({ 'factor' => factor, 'program-point' => ppref.data })
      self
    end
    # Flow fact builders
    def FlowFact.block_frequency(scope, block, freq, fact_context, classification)
      flowfact = FlowFact.new(fact_context.dup)
      flowfact['scope'] = scope.data
      flowfact['op'] = 'less-equal'
      flowfact['rhs'] = freq.max
      flowfact['classification'] = classification # block-* or infeasible-*
      flowfact.add_term(block.ref)
    end
    def FlowFact.calltargets(scope, cs, receiverset, fact_context, classification)
      flowfact = FlowFact.new(fact_context.dup)
      flowfact['scope'] = scope.data
      flowfact['op'] = 'equal'
      flowfact['rhs'] = 0
      flowfact['classification'] = classification # calltargets-*
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

    # Dynamic classification and simplification of special purpose flowfacts

    # return [scope,block,freq] if this flow fact constraints the frequency
    # of a single block (nil otherwise)
    def get_block_frequency_bound
      return nil unless @data['lhs'].length == 1
      term = @data['lhs'].first
      return nil unless term['factor'] == 1
      [@data['scope'], term['program-point'], @data['rhs']]
    end

    # return [scope,cs,targets] if this is a calltarget-* flowfact
    def get_calltargets
      callsite_candidate = @data['lhs'].select { |term|
        term['factor'].abs == 1 && term['program-point']['instruction']
      }
      return nil unless callsite_candidate.length == 1
      callsite = callsite_candidate.first['program-point']
      opposite_factor = callsite_candidate.first['factor']
      targets = []
      @data['lhs'].each { |term|
        next if term == callsite_candidate.first
        return nil unless term['factor'] == -opposite_factor
        return nil if term['program-point'].keys != ['function']
        targets.push(term['program-point']['function'])
      }
      [@data['scope'], callsite, targets]
    end
  end

  # timing entries are used to record WCET analysis results or measurement results
  class TimingEntry < Proxy
    def initialize(scope, cycles, context)
      @data = context.dup
      @data['scope'] ||= scope.data
      @data['cycles'] = cycles
    end
    def to_s
      @data.to_s
    end
  end
# end of module PML
end
