
#
# platin toolkit
#
# program information: flowfacts, timing, memory accesses
#
require 'core/utils'
require 'core/pmlbase'
require 'core/context'
require 'core/program'
require 'core/symbolic_expr'

module PML

  # Flow fact classification and selection
  class FlowFactClassifier

    def initialize(pml)
      @pml = pml
    end

    # FIXME: cache
    def classify(ff)
      c = OpenStruct.new
      # context-independent loop bound
      c.is_loop_bound = ! ff.get_loop_bound.nil?
      # context-independent block infeasibility
      c.is_infeasible = ! ff.get_block_infeasible.nil?
      # context-independent calltarget restriction
      (_,cs,_) = ff.get_calltargets
      c.is_indirect_calltarget = cs && cs.programpoint.unresolved_call?

      # rt: involves machine-code only function (usually from compiler-rt or operating system)
      if @pml
        mcofs = @pml.machine_code_only_functions
        c.is_rt = ff.lhs.any? { |term|
                    term.programpoint.function &&
                    mcofs.include?(term.programpoint.function.label)
                  }
      else
        c.is_rt = false
      end

      c.is_minimal = c.is_loop_bound || c.is_infeasible || c.is_indirect_calltarget
      c.is_local   = ff.local?
      c
    end

    def classification_group(ff)
      c = classify(ff)
      group_for_classification(c)
    end

    def group_for_classification(c)
      s = if c.is_loop_bound
            "loop-bound"
          elsif c.is_infeasible
            "infeasible"
          elsif c.is_indirect_calltarget
            "indirect-call-target"
          elsif c.is_local
            "local"
          else
            "global"
          end
      s = "#{s}-rt" if c.is_rt
      s
    end

    def included?(ff, profile)
      c = classify(ff)
      return true if profile == "all"
      case profile
      when "minimal"    then c.is_minimal
      when "local"      then c.is_minimal || c.is_local
      # FIXME: indirect calltargets are needed on MC level to build callgraph
      when "rt-support-all"   then c.is_rt || c.is_indirect_calltarget
      when "rt-support-local" then (c.is_rt && (c.is_local || c.is_minimal)) || c.is_indirect_calltarget
      when "rt-support-minimal" then (c.is_rt && c.is_minimal) || c.is_indirect_calltarget
      else raise Exception.new("Bad Flow-Fact Selection Profile: #{profile}")
      end
    end
  end

  # List of flowfacts (modifiable)
  class FlowFactList < PMLList
    extend PMLListGen
    pml_list(:FlowFact,[],[:origin])
    # customized by_origin implementation
    def by_origin(origin)
      if origin.kind_of?(String)
        @list.select { |ff| origin == ff.origin }
      else
        @list.select { |ff| origin.include?(ff.origin) }
      end
    end

    def add_copies(flowfacts, new_origin)
      copies = []
      flowfacts.each { |ff|
        ff_copy = ff.deep_clone
        ff_copy.origin = new_origin
        add(ff_copy)
        copies.push(ff_copy)
      }
      copies
    end

    def reject!
      rejects = []
      @list.reject! { |ff| r = yield ff ; rejects.push(r); r }
      data.reject! { |ff| rejects.shift }
    end

    def filter(pml, ff_selection, ff_srcs, ff_levels, exclude_symbolic = false)
      classifier = FlowFactClassifier.new(pml)
      @list.select { |ff|
        # skip if level does not match
        if ! ff_levels.include?(ff.level)
          false
        # skip if source is not included
        elsif ff_srcs != "all" && ! ff_srcs.include?(ff.origin)
          false
        elsif ! classifier.included?(ff, ff_selection)
          false
        elsif exclude_symbolic && ! ff.rhs.constant?
          false
	# always filter unknown bounds since we cannot handle them in the analyses
	elsif ff.rhs.kind_of?(SEUnknown)
	  false
        else
          true
        end
      }
    end

    def stats(pml)
      classifier = FlowFactClassifier.new(pml)
      by_level = {}
      @list.each { |ff|
        klass = classifier.classification_group(ff)
        by_origin = (by_level[ff.level] ||= {})
        by_origin[:cnt] = (by_origin[:cnt] || 0) + 1
        by_group = (by_origin[ff.origin] ||= {})
        by_group[:cnt] = (by_group[:cnt] || 0) + 1
        by_klass = (by_group[klass] ||= {})
        by_klass[:cnt] = (by_klass[:cnt] || 0) + 1
      }
      by_level
    end

    def dump_stats(pml, io=$stderr)
      io.puts "Flow-Facts, classified"
      stats(pml).each { |level,by_group|
        io.puts " #{level.to_s.ljust(39)} #{by_group[:cnt]}"
        by_group.each { |group,by_klass|
          next if group == :cnt
          io.puts "   #{group.to_s.ljust(37)} #{by_klass[:cnt]}"
          by_klass.each { |klass,stats|
            next if klass == :cnt
            io.puts "     #{klass.to_s.ljust(35)} #{stats[:cnt]}"
          }
        }
      }
    end
  end

  # List of Terms
  class TermList < PMLList
    extend PMLListGen
    pml_list(:Term,[])
  end

  # Term (ProgramPoint, Factor)
  # Immutable
  class Term < PMLObject
    attr_reader :factor, :ppref
    def initialize(pp,factor,data=nil)
      pp = ContextRef.new(pp, Context.empty) if pp.kind_of?(ProgramPoint)
      assert("Term#initialize: pp not a programpoint reference") { pp.kind_of?(ContextRef) }
      assert("Term#initialize: not a context-sensitive reference: #{pp} :: #{pp.class}") { pp.kind_of?(ContextRef) }
      @ppref, @factor = pp,factor
      set_yaml_repr(data)
    end
    def context
      @ppref.context
    end
    def programpoint
      @ppref.programpoint
    end

    # pp and factor are immutable, no clone necessary
    def deep_clone
      Term.new(@ppref, @factor)
    end

    def to_s
      "#{@factor} #{ppref}"
    end
    def to_pml
      { 'factor' => @factor, 'program-point' => @ppref.data }
    end
    def Term.from_pml(mod,data)
      Term.new(ContextRef.from_pml(mod,data['program-point']), data['factor'])
    end
  end

  # Flow Fact utility class
  # Kind of flow facts of interest
  # validity: * analysis-context ... flow fact is valid for the analyzed program
  #           * scope            ... flow fact is valid for each execution of its scope
  # scope:    * function,loop    ... flow fact applies to every execution of the scope
  # general:  * edges            ... relates CFG edges
  #           * blocks           ... relates CFG blocks
  #           * calltargets      ... relates call-sites and function entries
  # special:  * infeasible       ... specifies code (blocks) not executed
  #           * header           ... specifies bound on loop header
  #           * backedges        ... specifies bound of backedges
  class FlowFact < PMLObject
    attr_reader :scope, :lhs, :op, :rhs, :attributes
    include ProgramInfoObject

    def initialize(scope, lhs, op, rhs, attributes, data = nil)
      scope = ContextRef.new(scope, Context.empty) if scope.kind_of?(ProgramPoint)
      assert("scope not a reference") { scope.kind_of?(ContextRef) }
      assert("lhs not a list proxy") { lhs.kind_of?(PMLList) }
      assert("lhs is not a list of terms") { lhs.empty? || lhs[0].kind_of?(Term) }

      @scope, @lhs, @op, @rhs = scope, lhs, op, rhs
      @rhs = SEInt.new(@rhs) if rhs.kind_of?(Integer)
      @attributes = attributes
      raise Exception.new("No level attribute!") unless level
      set_yaml_repr(data)
    end

    # whether this flow fact has a symbolic constant (not fully supported)
    def symbolic_bound?
      ! @rhs.constant?
    end

    # string representation of the flow fact
    def to_s
      "#<FlowFact #{attributes.map {|k,v| "#{k}=#{v}"}.join(",")}, in #{scope}: #{lhs} #{op} #{rhs}>"
    end

    # deep clone: clone flow fact, lhs and attributes
    def deep_clone
      FlowFact.new(scope.dup, lhs.deep_clone, op, rhs, attributes.dup)
    end

    def classification_group(pml = nil)
      FlowFactClassifier.new(pml).classification_group(self)
    end

    def FlowFact.from_pml(pml, data)
      mod = pml.functions_for_level(data['level'])
      scope = ContextRef.from_pml(mod,data['scope'])
      lhs = TermList.new(data['lhs'].map { |t| Term.from_pml(mod,t) })
      attrs = ProgramInfoObject.attributes_from_pml(pml, data)
      rhs = SymbolicExpression.parse(data['rhs'])
      rhs = rhs.map_names { |ty,name|
        if ty == :variable # ok
          name
        elsif ty == :loop
          b = scope.function.blocks.by_name(name[1..-1]).loop
          unless b
            raise Exception.new("Unable to lookup loop: #{name} in #{b}")
          end
          b
        end
      }
      ff = FlowFact.new(scope, lhs, data['op'], rhs, attrs, data)
    end

    def to_pml
      assert("no 'level' attribute for flow-fact") { level }
      { 'scope' => scope.data,
        'lhs' => lhs.data,
        'op' => op,
        'rhs' => rhs.to_s,
      }.merge(attributes)
    end

    # Flow fact builders
    def FlowFact.block_frequency(scoperef, blockref, freq, attrs)
      terms = [ Term.new(blockref, 1) ]
      flowfact = FlowFact.new(scoperef, TermList.new(terms),'less-equal',freq.max, attrs.dup)
      flowfact
    end

    def FlowFact.calltargets(scoperef, csref, receivers, attrs)
      terms = [ Term.new(csref,1) ]
      receivers.each do |fref|
        terms.push(Term.new(fref,-1))
      end
      flowfact = FlowFact.new(scoperef,TermList.new(terms),'less-equal',0, attrs.dup)
      flowfact
    end

    def FlowFact.loop_bound(scope, bound, attrs)
      blockref = ContextRef.new(scope.programpoint.loopheader, Context.empty)
      flowfact = FlowFact.new(scope, TermList.new([Term.new(blockref,1)]), 'less-equal',
                              bound, attrs.dup)
      flowfact
    end

    def FlowFact.inner_loop_bound(scoperef, blockref, bound, attrs)
      flowfact = FlowFact.new(scoperef, TermList.new([Term.new(blockref,1)]), 'less-equal',
                              bound, attrs.dup)
      flowfact
    end

    def globally_valid?(entry_function)
      # local relative flow facts are globally valid
      return true if local? && rhs.constant? && rhs.to_i == 0
      # otherwise, the scoep has to be the entry function
      return scope.programpoint.kind_of?(Function) &&
             scope.function == entry_function &&
             scope.context.empty?
    end

    def local?
      lhs.all? { |term| term.programpoint.function &&
                        term.programpoint.function == scope.function }
    end

    def loop_bound?
      ! get_loop_bound.nil?
    end

    def loop_scope?
      scope.programpoint.kind_of?(Loop)
    end

    def context_sensitive?
      return true if lhs.any? { |term| ! term.context.empty? }
      return ! scope.context.empty?
    end

    def references_empty_block?
      lhs.any? { |t| t.programpoint.kind_of?(Block) && t.programpoint.instructions.empty? }
    end

    def references_edges?
      lhs.any? { |t| t.programpoint.kind_of?(Edge) }
    end

    def blocks_constraint?
      lhs.all? { |t| t.programpoint.kind_of?(Block) }
    end

    # if this constraint is a loop bound, return loop scope and bound
    def get_loop_bound
      s,b,rhs = get_block_frequency_bound
      return nil unless s
      return nil unless s.programpoint.kind_of?(Loop)
      return nil unless s.programpoint.loopheader == b.programpoint.block && b.context.empty?
      [s,rhs]
    end

    # if this constraints marks a block infeasible,
    # return [scope,block]
    def get_block_infeasible
      s,b,rhs = get_block_frequency_bound
      return nil unless s
      return nil unless rhs.constant? && rhs.to_i == 0
      return [scope,b]
    end

    # if this is a flowfact constraining the frequency of a single block,
    # return [scope, block, freq]
    #  block  ... Block
    #  freq   ... Integer
    def get_block_frequency_bound
      return nil unless lhs.list.length == 1
      term = lhs.list.first
      return nil unless term.factor == 1
      return nil unless term.programpoint.kind_of?(Block)
      [scope, term.ppref, rhs]
    end

    # if this is a calltarget-* flowfact, return [scope, cs, targets]:
    #   cs      ... ContextRef <Instruction>
    #   targets ... [Function]
    def get_calltargets
      callsite_candidate = lhs.list.select { |term|
        term.factor == 1 && term.programpoint.kind_of?(Instruction)
      }
      return nil unless callsite_candidate.length == 1
      callsite_ref = callsite_candidate.first.ppref
      opposite_factor = -1
      targets = []
      lhs.each { |term|
        next if term == callsite_candidate.first
        return nil unless term.factor == opposite_factor
        return nil unless term.programpoint.kind_of?(Function)
        targets.push(term.programpoint)
      }
      [scope, callsite_ref, targets]
    end
  end

  # List of value analysis entries (modifiable)
  class ValueFactList < PMLList
    extend PMLListGen
    pml_list(:ValueFact,[],[:origin])
  end

  class ValueSet < PMLList
    extend PMLListGen
    pml_list(:ValueRange)
    def dup
      ValueSet.new(@list.dup)
    end
  end

  class ValueRange < PMLObject
    attr_reader :symbol, :min, :max
    def initialize(min, max, symbol, data =nil)
      @min, @max, @symbol = min, max, symbol
      set_yaml_repr(data)
      raise Exception.new("Bad ValueRange: #{self}") unless @min <= @max if @min
    end
    def inspect
      "ValueRange<min=#{min.inspect},max=#{max.inspect},symbol=#{symbol}>"
    end
    def to_s
      symbol.to_s + (range ? range.to_s : '')
    end
    def range
      return nil unless max
      Range.new(min,max)
    end
    def ValueRange.from_pml(_,data)
      ValueRange.new(data['min'],data['max'],data['symbol'],data)
    end
    def to_pml
      { 'min' => @min, 'max' => @max, 'symbol' => @symbol }.delete_if { |_,v| v.nil? }
    end
  end

  # value facts record results from value analysis (at the binary or bitcode level)
  #
  # program-point: pp
  # variable: mem-address-read, mem-address-write, register name
  # width: int
  # values: list of { min: x, max: x }
  class ValueFact < PMLObject
    attr_reader :attributes, :ppref, :variable, :width, :values
    include ProgramInfoObject
    def initialize(ppref, variable, width, values, attrs, data = nil)
      assert("ValueFact#initialize: program point reference has wrong type (#{ppref.class})") { ppref.kind_of?(ContextRef) }
      @ppref, @variable, @width, @values = ppref, variable, width, values
      @attributes = attrs
      set_yaml_repr(data)
    end
    def programpoint
      @ppref.programpoint
    end
    def ValueFact.from_pml(pml, data)
      fs = pml.functions_for_level(data['level'])
      ValueFact.new(ContextRef.from_pml(fs,data['program-point']),
                    data['variable'], data['width'],
                    ValueSet.from_pml(fs,data['values']),
                    ProgramInfoObject.attributes_from_pml(pml, data),
                    data)
    end
    def to_pml
      { 'program-point' => @ppref.data,
        'variable' => @variable,
        'width' => @width,
        'values' => @values.data }.merge(attributes)
    end

    # string representation of the value fact
    def to_s
      vs = values.map { |vr| vr.to_s }.join(", ")
      "#<ValueFact #{attributes.map {|k,v| "#{k}=#{v}"}.join(",")}, at #{ppref}: #{variable}#{"[width=#{width}]" if width} \\in {#{vs}}>"
    end

  end

  # List of timing entries (modifiable)
  class TimingList < PMLList
    extend PMLListGen
    pml_list(:TimingEntry, [], [:origin])
  end
  class Profile < PMLList
    extend PMLListGen
    pml_list(:ProfileEntry, [:reference], [])
  end
  class ProfileEntry < PMLObject
    attr_reader :reference, :cycles, :wcetfreq, :criticality, :wcet_contribution
    def initialize(reference, cycles, wcetfreq, wcet_contribution, criticality = nil, data = nil)
      @reference, @cycles, @wcetfreq, @wcet_contribution, @criticality =
       reference,  cycles,  wcetfreq,  wcet_contribution,  criticality
      set_yaml_repr(data)
    end
    def ProfileEntry.from_pml(fs, data)
      ProfileEntry.new(ContextRef.from_pml(fs,data['reference']), data['cycles'],
                       data['wcet-frequency'], data['wcet-contribution'], data['criticality'], data)
    end
    def criticality=(c)
      @criticality = c
      @data['criticality'] = c if @data
    end
    def to_pml
      { 'reference' => reference.data, 'cycles' => cycles, 'wcet-frequency' => wcetfreq,
        'criticality' => criticality, 'wcet-contribution' => wcet_contribution }.delete_if {|k,v| v.nil? }
    end
    def to_s
      data
    end
  end

  # timing entries are used to record WCET analysis results or measurement results
  class TimingEntry < PMLObject
    attr_reader :cycles, :scope, :profile
    attr_reader :attributes
    include ProgramInfoObject

    def initialize(scope, cycles, profile, attrs, data = nil)
      @scope = scope
      @scope = ContextRef.new(@scope, Context.empty) if scope.kind_of?(ProgramPoint)
      assert("TimingEntry#initialize: not a programpoint reference") { @scope.kind_of?(ContextRef) }
      @cycles = cycles
      @profile = profile
      @attributes = attrs
      set_yaml_repr(data)
    end
    def profile=(p)
      @profile = p
      @data['profile'] = @profile.data if @data
    end
    def TimingEntry.from_pml(pml, data)
      fs = pml.functions_for_level(data['level'])
      profile = data['profile'] ? Profile.from_pml(fs, data['profile']) : nil
      TimingEntry.new(ContextRef.from_pml(fs,data['scope']), data['cycles'],
                      profile,
                      ProgramInfoObject.attributes_from_pml(pml,data), data)
    end
    def to_pml
      pml = { 'scope' => @scope.data, 'cycles' => @cycles }.merge(attributes)
      pml['profile'] = @profile.data if @profile
      pml
    end

    def to_s
      "#<TimingEntry #{attributes.map {|k,v| "#{k}=#{v}"}.join(",")}, in #{scope}: #{cycles} cycles>"
    end
  end

  # Graph structure representing stack cache analysis results
  class SCAGraph < PMLObject
    attr_reader :nodes,:edges,:pml
    def initialize(pml, data)
      set_yaml_repr(data)
      @pml = pml
      @nodes = SCANodeList.new(data['nodes'] || [], self)
      @edges = SCAEdgeList.new(data['edges'] || [], self)
    end
  end

  # List of stack cache graph nodes
  class SCANodeList < PMLList
    extend PMLListGen
    pml_name_index_list(:SCAEdge)
    def initialize(data, tree)
      @list = data.map { |n| SCANode.new(n,tree.pml) }
      set_yaml_repr(data)
    end
  end

  # List of stack cache graph edges
  class SCAEdgeList < PMLList
    extend PMLListGen
    pml_name_index_list(:SCANode)
    def initialize(data, tree)
      @list = data.map { |n| SCAEdge.new(n) }
      set_yaml_repr(data)
    end
  end

  # Stack cache graph node
  class SCANode < PMLObject
    attr_reader :id,:function,:size
    def initialize(data, pml)
      set_yaml_repr(data)
      @id = data['id']
      @function = pml.machine_functions.by_label(data['function'])
      @size = data['spillsize']
    end
    def to_s
      "#{id}:#{function}:#{size}"
    end
    def qname
      return @id
    end
  end

  # Stack cache graph edge
  class SCAEdge < PMLObject
    attr_reader :src,:dst,:block,:inst
    def initialize(data)
      set_yaml_repr(data)
      @src = data['src']
      @dst = data['dst']
      @block = data['callblock']
      @inst = data['callindex']
    end
    def to_s
      "#{src}:#{block}:#{inst}->#{dst}"
    end
  end

# end of module PML
end
