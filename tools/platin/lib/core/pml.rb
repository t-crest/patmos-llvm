#
# PLATIN tool set
#
# PML data format classes
#
# Provide smart accessors, caching, etc.
#
module PML

  RE_HEX=/[0-9A-Fa-f]/

  def assert(msg)
    unless yield
      pnt = Thread.current.backtrace[1]
      $stderr.puts ("#{$0}: Assertion failed in #{pnt}: #{msg}")
      puts "    "+Thread.current.backtrace[1..-1].join("\n    ")
      exit 1
    end
  end

  # TODO: refactor label stuff
  def parse_mbb_label(label)
    label =~ /\A\.LBB(\d+)_(\d+)$/
    [$1.to_i, $2.to_i] if $1
  end

  def dquote(str)
    '"' + str + '"'
  end

  def merge_ranges(r1,r2=nil)
    assert("first argument is nil") { r1 }
    r1=Range.new(r1,r1) unless r1.kind_of?(Range)
    return r1 unless r2
    [r1.min,r2.min].min .. [r1.max,r2.max].max
  end

  # calculate the reachable set from entry,
  # where the provided block needs to compute
  # the successors of an item
  def reachable_set(entry)
    reachable = Set.new
    todo = [entry]
    while !todo.empty?
      item = todo.pop
      next if reachable.include?(item)
      reachable.add(item)
      successors = yield item
      successors.each do |succ|
        todo.push(succ)
      end
    end
    reachable
  end

  # Proxy for YAML data
  class Proxy
    # The PML data corresponding to the object
    attr_reader :data
  end

  # Proxy for (read-only) lists
  class ListProxy
    attr_reader :list
    def to_s
      list.to_s
    end
    # delegator to list (which should be frozen)
    def method_missing(method, *args, &block)
      list.send(method, *args, &block)
    end
    def lookup(dict,key,name)
      v = dict[key]
      raise Exception.new("#{self.class}#by_#{name}: No object with key '#{key}' in #{dict.inspect}") unless v
      v
    end
    def lookup_optional(dict,key,name)
      begin
        return lookup(dict,key,name)
      rescue Exception => detail
        return nil
      end
    end
    def add_lookup(dict,key,val,name,opts={})
      return if ! key && opts[:ignore_if_missing]
      raise Exception.new("#{self.class}#by_#{name}: Duplicate object with key #{key}: #{val} and #{dict[key]}") if dict[key]
      dict[key] = val
    end
  end

  # Lists where elements can be queried by name and qualified name
  module NameIndexList
    def by_name(name)
      build_name_index unless @named
      lookup(@named, name, "name")
    end
    def by_qname(name)
      build_name_index unless @named
      lookup(@qnamed, name, "qname")
    end
    def build_name_index
      @named, @qnamed = {}, {}
      list.each do |v|
        add_lookup(@named, v.name, v, "name")
        add_lookup(@qnamed, v.qname, v, "qname")
      end
    end
  end

  # architectures
  class Architecture
    @@register = {}
    def Architecture.register(archname,klass)
      die("architecture #{archname} already registered to #{@@register[archname]}") if @@register[archname]
      @@register[archname] = klass
    end
    def Architecture.from_triple(triple)
      archname = triple.first
      die("unknown architecture #{triple} (#{@@register})") unless @@register[archname]
      @@register[archname].new(triple)
    end
    require 'arch/patmos'
  end

  # class providing convenient accessors and additional program information derived
  # from PML files
  class PMLDoc < Proxy
    attr_reader :arch, :bitcode_functions,:machine_functions,:relation_graphs,:flowfacts,:timing

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
      triple = @data['triple'].split('-')
      @arch = Architecture.from_triple(triple)
      @bitcode_functions = FunctionList.new(@data['bitcode-functions'] || [])
      @machine_functions = FunctionList.new(@data['machine-functions'] || [])
      @relation_graphs   = RelationGraphList.new(@data['relation-graphs'] || [], @bitcode_functions, @machine_functions)
      @data['flowfacts'] ||= []
      @flowfacts = FlowFactList.from_pml(self, @data['flowfacts'])
      @data['timing'] ||= []
      @timing = TimingList.from_pml(self, @data['timing'])
    end

    def to_s
      "PMLDoc{bitcode-functions: |#{bitcode_functions.length}|, machine-functions: |#{machine_functions.length}"+
        ", flowfacts: |#{flowfacts.length}|}, timing: |#{timing.length}|"
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
      final = @data.clone
      final.delete("flowfacts") if @data["flowfacts"] == []
      final.delete("timing") if @data["timing"] == []
      io.write(YAML::dump(final))
    end

    def delay_slots
      @arch.delay_slots
    end

    def machine_code_only_functions
      %w{_start _exit exit abort __ashldi3 __adddf3 __addsf3 __divsi3 __divdf3 __divsf3 __eqdf2 __eqsf2 __extendsfdf2} +
        %w{__fixdfdi __fixdfsi __fixsfdi __fixsfsi __fixunsdfdi __fixunsdfsi __fixunssfdi __fixunssfsi __floatdidf __floatdisf} +
        %w{__floatsidf __floatsisf __floatundidf __floatundisf __floatunsidf __floatunsisf __gedf2 __gesf2 __gtdf2 __gtsf2} +
        %w{__ledf2 __lesf2 __lshrdi3 __ltdf2 __ltsf2 __muldf3 __mulsf3 __nedf2 __nesf2 __subdf3 __subsf3 __truncdfsf2 __unorddf2 __unordsf2} +
        %w{memcpy memmove memset}
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
            die "Mismatch in non-list attribute #{k}: #{merged_doc[k]} and #{doc[k]}"
          end
        end
      end
      merged_doc
    end
  end

  class Reference < Proxy
    attr_reader :qname
    def Reference.from_pml(functions, data)
      assert("PML Reference: no function attribute") { data['function'] }
      function = functions.by_name(data['function'])
      if block = data['block']
        block = function.blocks.by_name(block)
        if index = data['instruction']
          ins = block.instructions[index]
          return InstructionRef.new(ins,data)
        else
          return BlockRef.new(block,data)
        end
      elsif loop = data['loop']
        loop = function.blocks.by_name(loop)
        return LoopRef.new(loop, data)
      elsif src=data['edgesource']
        bb_src = function.blocks.by_name(src)
        bb_dst = function.blocks.by_name(data['edgetarget'])
        return EdgeRef.new(bb_src, bb_dst, data)
      else
        return FunctionRef.new(function,data)
      end
    end
  end

  # Qualified name for functions
  class FunctionRef < Reference
    attr_reader :function, :qname
    def initialize(function, data=nil)
      @function = function
      @qname = function.qname
      @data = data || { 'function' => function.name }
    end
  end

  # Qualified name for blocks
  class BlockRef < Reference
    attr_reader :function, :block, :qname
    def initialize(block, data = nil)
      @block = block
      @function = block.function
      @qname = block.qname
      @data = data || { 'function' => block.function.name, 'block' => block.name }
    end
  end

  # Qualified name for loops
  class LoopRef < Reference
    attr_reader :function, :loopblock, :qname
    def initialize(block, data = nil)
      @loopblock = block
      @function = block.function
      @qname = block.qname
      @data = data || { 'function' => loopblock.function.name, 'loop' => loopblock.name }
    end
  end

  class EdgeRef < Reference
    attr_reader :source, :target, :qname
    def initialize(source, target, data = nil)
      assert("PML Edge: source and target function need to match") { source.function == target.function }
      @source, @target = source, target
      @data = data || { 'function' => source.function.name, 'edgesource' => source.name, 'edgetarget' => target.name }
      @name = "#{source.name}->#{target.name}"
      @qname = "#{source.qname}->#{target.name}"
      @hash = @qname.hash
    end
    def function
      source.function
    end
    def [](k)
      @data[k]
    end
    def to_s
      qname
    end
    def hash; @hash ; end
  end

  # Qualified name for instructions
  class InstructionRef < Reference
    attr_reader :function, :block, :instruction, :qname
    def initialize(instruction, data = nil)
      @instruction = instruction
      @block, @function = instruction.block, instruction.function
      @qname = instruction.qname
      @data = data || { 'function' => instruction.function.name,
        'block' => instruction.block.name, 'instruction' => instruction.name }
    end
    def block
      instruction.block
    end
  end

  # List of functions in the program
  class FunctionList < ListProxy
    include NameIndexList
    def initialize(data)
      @list = data.map { |f| Function.new(f) }
      @data = data
      build_lookup
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

    def [](name)
      by_name(name)
    end
    def by_address(addr)
      lookup(@address, addr, "address")
    end
    def by_label(label)
      lookup(@labelled, label, "label")
    end
    def build_lookup
      @address = {}
      @labelled = {}
      @list.each do |v|
        add_lookup(@labelled,v.label,v,"label",:ignore_if_missing => true)
        add_lookup(@address,v.address,v,"address",:ignore_if_missing => true)
      end
    end
  end

  # List of PML basic blocks in a function
  class BlockList < ListProxy
    include NameIndexList
    def initialize(function, data)
      @list = data.map { |b| Block.new(function, b) }
      @data = data
      @data.freeze
    end
    def first
      @list.first
    end
    def [](name)
      by_name[name]
    end
  end

  # List of PML instructions in a block
  class InstructionList < ListProxy
    include NameIndexList
    def initialize(block, data)
      @list = data.map { |i| Instruction.new(block, i) }
      @data = data
      @data.freeze
    end
    def [](index)
      @list[index]
    end
  end

  # References to Program Points (functions, blocks, instructions)
  class ProgramPointProxy < Proxy
    attr_reader :name, :qname
    def address
      @data['address']
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
      @qname = name
      @hash = name.hash
      @loops = []
      @blocks = BlockList.new(self, @data['blocks'])
      blocks.each do |block|
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
      @data['label'] || @data['mapsto'] || blocks.first.label
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
    def initialize(function,mbb)
      @data = mbb
      @function = function
      @name = @data['name']
      @qname = "#{function.name}/#{@name}"
      @hash = qname.hash

      loopnames = @data['loops'] || []
      @loopnest = loopnames.length
      @is_loopheader = loopnames.first == self.name

      die("No instructions in #{@name}") unless @data['instructions']
      @instructions = InstructionList.new(self, @data['instructions'])
    end
    def [](k)
      assert("Do not access instructions via []") { k != 'instructions' }
      assert("Do not access predecessors/successors directly") { k != 'predecessors' && k != 'successors' }
      assert("Do not access loops directly") { k != 'loops' }
      @data[k]
    end
    # loops: not ready at initialization time
    def loops
      return @loops if @loops
      @loops = (@data['loops']||[]).map { |l| function.blocks.by_name(l) }
    end

    # block predecessors; not ready at initialization time
    def predecessors
      return @predecessors if @predecessors
      @predecessors = (@data['predecessors']||[]).map { |s| function.blocks.by_name(s) }.uniq.freeze
    end
    # block successors; not ready at initialization time
    def successors
      return @successors if @successors
      @successors = (@data['successors']||[]).map { |s| function.blocks.by_name(s) }.uniq.freeze
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
    def hash; @hash ; end
    def loopheader? ; @is_loopheader ; end
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
    attr_reader :data, :block
    def initialize(block,ins)
      @block = block
      @data = ins
      @name = index
      @qname = "#{block.qname}/#{@name}"
      @hash = @qname.hash
    end
    def ref
      InstructionRef.new(self)
    end
    def callees
      @data['callees'] || []
    end
    def unresolved_call?
      callees.include?("__any__")
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

  # List of relation graphs (unmodifiable)
  class RelationGraphList < ListProxy
    def initialize(data, srclist, dstlist)
      @list = data.map { |rg| RelationGraph.new(rg, srclist, dstlist) }
      @data = data
      build_lookup
    end
    def has_named?(name, level)
      ! @named[level][name].nil?
    end
    def by_name(name, level)
      assert("RelationGraphList#by_name: level != :src,:dst") { [:src,:dst].include?(level) }
      lookup(@named[level], name, "#{level}-name")
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
  class RelationNodeList < ListProxy
    include NameIndexList
    def initialize(data, rg)
      @list = data.map { |n| RelationNode.new(n, rg) }
      @data = data
      @data.freeze
    end
  end


  # Relation Graphs
  class RelationGraph < Proxy
    attr_reader :src_functions, :dst_functions, :src, :dst, :nodes
    def initialize(data,src_funs,dst_funs)
      @data = data
      @src_functions, @dst_functions = src_funs, dst_funs
      @src = src_funs.by_name(data['src']['function'])
      @dst = dst_funs.by_name(data['dst']['function'])
      @nodes = RelationNodeList.new(data['nodes'], self)
    end
    def get_function(level)
      level == :src ? @src : @dst
    end
    def qname
      "#{src.qname}<>#{dst.qname}"
    end
  end

  # Relation Graph node
  class RelationNode < Proxy
    attr_reader :name, :rg
    def initialize(data, rg)
      @data = data
      @rg = rg
      @name = data['name']
      @successors = {} # lazy initialization
    end
    def get_block(level)
      return nil unless @data["#{level}-block"]
      rg.get_function(level).blocks.by_name(@data["#{level}-block"])
    end
    # returns one out of [ :progress, :dst, :src, :entry, :exit ]
    def type
      @data['type'].to_sym
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
      @successors[level] = (@data["#{level}-successors"]||[]).map { |succ|
        @rg.nodes.by_name(succ)
      }.uniq
      @successors[level]
    end
    def qname
      "#{rg.qname}_#{name}"
    end
    def to_s
      "#{type}:#{qname}"
    end
    # Extract qname ~ id pattern and refactor as mixin
    def ==(other)  ; qname == other.qname ; end
    def <=>(other) ; qname <=> other.qname ; end
    def eql?(other); self == other ; end
    def hash; qname.hash ; end
  end

  # Flow fact selector
  class FlowFactSelection
    MINIMAL_FLOWFACT_TYPES = %w{loop-local calltargets-global infeasible-global}
    def initialize(pml, profile)
      @pml, @profile = pml, profile
    end
    def include?(ff)
      return true if @profile == "all"
      is_loop       = ff.classification == "loop-local"
      is_infeasible = ff.classification == "infeasible-global"
      (_,cs,_)      = ff.get_calltargets
      is_calltarget = cs && cs.instruction.unresolved_call?
      is_rt         = ff.lhs.any? { |term| @pml.machine_code_only_functions.include?(term.ppref.function.label) }
      is_minimal    = is_loop || is_infeasible || is_calltarget
      is_local      = ff.lhs.all? { |term| term.ppref.function == ff.scope.function }
      case @profile
      when "minimal"    then is_minimal
      when "local"      then is_minimal || is_local
      when "rt-support" then is_rt || is_calltarget # FIXME: calltargets not supported on bitcode level yet
      else raise Exception.new("Bad Profile #{@profile}")
      end
    end
  end

  # List of flowfacts (modifiable)
  class FlowFactList < ListProxy
    def initialize(list, data = nil)
      assert("list is nil") { list }
      @list = list
      @data = data ? data : list.map { |ff| ff.to_pml }
      build_index
    end
    def FlowFactList.from_pml(pml, data)
      FlowFactList.new(data.map { |d| FlowFact.from_pml(pml,d) }, data)
    end
    def add(ff)
      @list.push(ff)
      @data.push(ff.data)
      add_index(ff)
    end

    def filter(pml, ff_selection, ff_srcs, ff_levels)
      selector = FlowFactSelection.new(pml, ff_selection)
      @list.select { |ff|
        # skip if level does not match
        if ! ff_levels.include?(ff.level)
          false
        # skip if source is not included
        elsif ff_srcs != "all" && ! ff_srcs.include?(ff.origin)
          false
        elsif ! selector.include?(ff)
          false
        else
          true
        end
      }
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

  # List of Terms
  class TermList < ListProxy
    def initialize(list,data=nil)
      @list = list
      @data = data ? data : to_pml
    end
    def deep_clone
      list = @list.map { |v| v.deep_clone }
      TermList.new(list)
    end
    def to_pml
      list.map { |t| t.to_pml }
    end
  end

  # Term (ProgramPoint, Factor)
  class Term < Proxy
    attr_reader :ppref, :factor
    def initialize(ppref,factor)
      assert("Term#initialize: not a reference: #{ppref}") { ppref.kind_of?(Reference) }
      @ppref,@factor = ppref,factor
      @data = data ? data : to_pml
    end
    # ppref and factor are immutable
    def deep_clone
      Term.new(ppref, factor)
    end
    def to_pml
      { 'factor' => factor, 'program-point' => ppref.data }
    end
    def Term.from_pml(mod,data)
      Term.new(Reference.from_pml(mod,data['program-point']), data['factor'])
    end
  end

  # Flow Fact utility class
  # Kind of flow facts of interest
  # validity: * analysis-context ... flow fact is valid in the analysis context
  #           * scope            ... flow fact is valid for each execution of its scope
  # scope:    * function,loop    ... flow fact applies to every execution of the scope
  # general:  * edges            ... relates CFG edges
  #           * blocks           ... relates CFG blocks
  #           * calltargets      ... relates call-sites and function entries
  # special:  * infeasible       ... specifies code (blocks) not executed
  #           * header           ... specifies bound on loop header
  #           * backedges        ... specifies bound of backedges
  class FlowFact < Proxy
    ATTRIBUTES = %w{classification level origin}
    CLASSIFICATION_OBJECTS = %w{block loop calltargets infeasible}
    CLASSIFICATION_SCOPES  = %w{local function global}
    REFINE_FLOWFACT_TYPES = %w{calltargets-global infeasible-global}
    MINIMAL_FLOWFACT_TYPES = %w{loop-local calltargets-global infeasible-global}
    attr_reader :scope, :lhs, :op, :rhs
    def initialize(scope, lhs, op, rhs, data = nil)
      assert("scope not a reference") { scope.kind_of?(Reference) }
      assert("lhs not a list proxy") { lhs.kind_of?(ListProxy) }
      assert("lhs is not a list of terms") { lhs.empty? || lhs[0].kind_of?(Term) }

      @scope, @lhs, @op, @rhs = scope, lhs, op, rhs
      @attributes = {}
      if data
        @data = data
        @data.each do |k,v|
          add_attribute(k,v) if ATTRIBUTES.include?(k)
        end
      else
        @data = to_pml
      end
    end
    # string repr
    def to_s
      "FlowFact<#{@attributes.map {|k,v| "#{k}=#{v}"}.join(",")},in #{scope}: #{lhs} #{op} #{rhs}>"
    end
    # clone flow fact, lhs and attributes
    def deep_clone
      ff = FlowFact.new(scope, lhs.deep_clone, op, rhs)
      @attributes.each do |k,v|
        ff.add_attribute(k,v)
      end
      ff
    end
    def FlowFact.from_pml(pml, data)
      mod = if data['level'] == 'bitcode'
              pml.bitcode_functions
            elsif data['level'] == 'machinecode'
              pml.machine_functions
            else
              raise Exception.new("Unsupported representation level: #{data['level']}")
            end
      scope = Reference.from_pml(mod,data['scope'])
      lhs = TermList.new(data['lhs'].map { |t| Term.from_pml(mod,t) })
      ff = FlowFact.new(scope, lhs, data['op'], data['rhs'], data)
    end
    def add_attribute(k,v)
      assert("Bad attribute #{k}") { ATTRIBUTES.include?(k) }
      @data[k] = v
      @attributes[k] = v
    end
    def add_attributes(attrs,moreattrs={})
      attrs.merge(moreattrs).each { |k,v| add_attribute(k,v) }
    end
    def classification
      @attributes['classification']
    end
    def level
      @attributes['level']
    end
    def origin
      @attributes ['origin']
    end
    def [](k)
      @attributes[k]
    end
    def to_pml
      { 'scope' => scope.data,
        'lhs' => lhs.to_pml,
        'op' => op,
        'rhs' => rhs,
      }.merge(@attributes)
    end

    # Flow fact builders
    def FlowFact.block_frequency(scoperef, block, freq, fact_context, classification)
      terms = [ Term.new(block.ref, 1) ]
      flowfact = FlowFact.new(scoperef, TermList.new(terms),'less-eqal',freq.max)
      flowfact.add_attributes(fact_context, 'classification' => classification)
      flowfact
    end

    def FlowFact.calltargets(scoperef, cs, receiverset, fact_context, classification)
      terms = [ Term.new(cs.ref, -1) ]
      receiverset.each do |function| 
        terms.push(Term.new(function.ref, 1))
      end
      flowfact = FlowFact.new(scoperef,TermList.new(terms),'equal',0)
      flowfact.add_attributes(fact_context, 'classification' => classification)
      flowfact
    end

    # Dynamic classification and simplification of special purpose flowfacts

    # if this is a flowfact constraining the frequency of a single block,
    # return [scope, block, freq]
    #  block  ... BlockRef
    #  freq   ... Integer
    def get_block_frequency_bound
      return nil unless lhs.list.length == 1
      term = lhs.list.first
      return nil unless term.factor == 1
      [scope, term.ppref, rhs]
    end

    # if this is a calltarget-* flowfact, return [scope, cs, targets]:
    #   cs      ... InstructionRef
    #   targets ... [FunctionRef]
    def get_calltargets
      callsite_candidate = lhs.list.select { |term|
        term.factor.abs == 1 && term.ppref.kind_of?(InstructionRef)
      }
      return nil unless callsite_candidate.length == 1
      callsite = callsite_candidate.first.ppref
      opposite_factor = callsite_candidate.first.factor
      targets = []
      lhs.each { |term|
        next if term == callsite_candidate.first
        return nil unless term.factor == -opposite_factor
        return nil unless term.ppref.kind_of?(FunctionRef)
        targets.push(term.ppref.function)
      }
      [scope, callsite, targets]
    end
  end

  # List of timing entries (modifiable)
  class TimingList < ListProxy
    def initialize(list, data = nil)
      assert("list is nil") { list }
      @list = list
      @data = data ? data : list.map { |ff| ff.to_pml }
      build_index
    end
    def TimingList.from_pml(pml, data)
      TimingList.new(data.map { |d| TimingEntry.from_pml(pml,d) }, data)
    end
    def add(te)
      @list.push(te)
      @data.push(te.data)
      add_index(te)
    end
    def by_origin(origin)
      lookup_optional(@by_origin, origin, "origin")
    end
    private
    def build_index
      @by_origin = {}
      @list.each { |te| add_index(te) }
    end
    def add_index(te)
      (@by_origin[te.origin]||=[]).push(te)
    end
  end

  # timing entries are used to record WCET analysis results or measurement results
  class TimingEntry < Proxy
    attr_reader :cycles, :scope
    def initialize(scope, cycles, context, data = nil)
      @scope = scope
      @cycles = cycles
      @context = context
      @data = data || to_pml
    end
    def origin
      data['origin']
    end
    ### XXX: code dup
    def TimingEntry.from_pml(pml, data)
      mod = if data['level'] == 'bitcode'
              pml.bitcode_functions
            elsif data['level'] == 'machinecode'
              pml.machine_functions
            else
              raise Exception.new("Unsupported representation level: #{data['level']}")
            end
      TimingEntry.new(Reference.from_pml(mod,data['scope']), data['cycles'], data, data)
    end
    def to_pml
      data = @context.clone
      data['scope'] = @scope.data
      data['cycles'] = @cycles
      data
    end
    def to_s
      @data.to_s
    end
  end
# end of module PML
end
