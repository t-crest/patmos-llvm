#
# platin toolkit
#
# program information: flowfacts, timing, memory accesses
#
require 'core/utils'
require 'core/pmlbase'
require 'core/context'
require 'core/program'

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
      (_,cs,_)      = ff.get_calltargets
      c.is_indirect_calltarget = cs && cs.instruction.unresolved_call?
      # rt: involves machine-code only function
      c.is_rt         = ff.lhs.any? { |term| @pml.machine_code_only_functions.include?(term.ppref.function.label) }
      c.is_minimal    = c.is_loop_bound || c.is_infeasible || c.is_indirect_calltarget
      c.is_local      = c.is_minimal || ff.lhs.all? { |term| term.ppref.function == ff.scope.function }
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
      when "local"      then c.is_local
      # FIXME: indirect calltargets are needed on MC level to build callgraph
      when "rt-support-all"   then c.is_rt || c.is_indirect_calltarget
      when "rt-support-local" then (c.is_rt && c.is_local) || c.is_indirect_calltarget
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

    def filter(pml, ff_selection, ff_srcs, ff_levels)
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
    def deep_clone
      self.dup # Terms are immutable
    end
  end

  # Term (ProgramPoint, Factor)
  # Immutable
  class Term < PMLObject
    attr_reader :factor, :pp
    def initialize(pp,factor,data=nil)
      pp = ContextRef.new(pp, Context.empty) if pp.kind_of?(Reference)
      assert("Term#initialize: not a context-sensitive reference: #{pp} :: #{pp.class}") { pp.kind_of?(ContextRef) }
      @pp,@factor = pp,factor
      set_yaml_repr(data)
    end
    def context
      @pp.context
    end
    def ppref
      @pp.reference
    end
    # pp and factor are immutable, we just dup them
    # to avoid sharing in the YAML file
    def deep_clone
      Term.new(pp.dup, factor)
    end
    def to_s
      "#{@factor} #{pp.qname}"
    end
    def to_pml
      { 'factor' => factor, 'program-point' => pp.data }
    end
    def Term.from_pml(mod,data)
      Term.new(ContextRef.from_pml(mod,data['program-point']), data['factor'])
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
  class FlowFact < PMLObject
    attr_reader :scope, :lhs, :op, :rhs, :attributes
    include ProgramInfoObject

    def initialize(scope, lhs, op, rhs, attributes, data = nil)
      scope = ContextRef.new(scope,Context.empty) if scope.kind_of?(Reference)
      assert("scope not a reference") { scope.kind_of?(ContextRef) }
      assert("lhs not a list proxy") { lhs.kind_of?(PMLList) }
      assert("lhs is not a list of terms") { lhs.empty? || lhs[0].kind_of?(Term) }

      # Do not handle symbolic loop bounds for now...
      if rhs.kind_of?(String) && rhs.strip =~ /\A\d+\Z/
        rhs = rhs.strip.to_i
      end
      @scope, @lhs, @op, @rhs = scope, lhs, op, rhs
      @attributes = attributes
      raise Exception.new("No level attribute!") unless level
      set_yaml_repr(data)
    end

    # whether this flow fact has a symbolic constant (not fully supported)
    def symbolic_bound?
      ! @rhs.kind_of?(Integer)
    end

    # string representation of the flow fact
    def to_s
      "FlowFact<#{attributes.map {|k,v| "#{k}=#{v}"}.join(",")},in #{scope}: #{lhs} #{op} #{rhs}>"
    end

    # deep clone: clone flow fact, lhs and attributes
    def deep_clone
      FlowFact.new(scope, lhs.deep_clone, op, rhs, attributes.dup)
    end
    def FlowFact.from_pml(pml, data)
      mod = pml.functions_for_level(data['level'])
      scope = Reference.from_pml(mod,data['scope'])
      lhs = TermList.new(data['lhs'].map { |t| Term.from_pml(mod,t) })
      attrs = ProgramInfoObject.attributes_from_pml(pml, data)
      ff = FlowFact.new(scope, lhs, data['op'], data['rhs'], attrs, data)
    end
    def to_pml
      assert("no 'level' attribute for flow-fact") { level }
      { 'scope' => scope.data,
        'lhs' => lhs.data,
        'op' => op,
        'rhs' => rhs,
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

    def FlowFact.loop_bound(scoperef, bound, attrs)
      blockref = ContextRef.new(BlockRef.new(scoperef.reference.loopblock),Context.empty)
      flowfact = FlowFact.new(scoperef, TermList.new([Term.new(blockref,1)]), 'less-equal',
                              bound, attrs.dup)
      flowfact
    end

    def references_empty_block?
      lhs.any? { |t| t.ppref.kind_of?(BlockRef) && t.ppref.block.instructions.empty? }
    end

    def references_edges?
      lhs.any? { |t| t.ppref.kind_of?(EdgeRef) }
    end

    def blocks_constraint?
      lhs.all? { |t| t.ppref.kind_of?(BlockRef) }
    end

    # if this constraint is a loop bound, return loop scope and bound
    def get_loop_bound
      s,b,rhs = get_block_frequency_bound
      return nil unless s
      return nil unless s.reference.kind_of?(LoopRef)
      return nil unless s.reference.loopblock == b.reference.block && b.context.empty?
      [s,rhs]
    end

    # if this constraints marks a block infeasible,
    # return [scope,block]
    def get_block_infeasible
      s,b,rhs = get_block_frequency_bound
      return nil unless s
      return nil unless rhs == 0
      return [scope,b]
    end

    # if this is a flowfact constraining the frequency of a single block,
    # return [scope, block, freq]
    #  block  ... BlockRef
    #  freq   ... Integer
    def get_block_frequency_bound
      return nil unless lhs.list.length == 1
      term = lhs.list.first
      return nil unless term.factor == 1
      return nil unless term.ppref.kind_of?(BlockRef)
      [scope, term.pp, rhs]
    end

    # if this is a calltarget-* flowfact, return [scope, cs, targets]:
    #   cs      ... ContextRef <InstructionRef>
    #   targets ... [FunctionRef]
    def get_calltargets
      callsite_candidate = lhs.list.select { |term|
        term.factor == 1 && term.ppref.kind_of?(InstructionRef)
      }
      return nil unless callsite_candidate.length == 1
      callsite_ref = callsite_candidate.first.pp
      opposite_factor = -1
      targets = []
      lhs.each { |term|
        next if term == callsite_candidate.first
        return nil unless term.factor == opposite_factor
        return nil unless term.ppref.kind_of?(FunctionRef)
        targets.push(term.ppref)
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
      ValueSet.new(@list.dup, data.dup)
    end
  end

  class ValueRange < PMLObject
      attr_reader :min,:max
    def initialize(min, max, data =nil)
      @min, @max = min, max
      set_yaml_repr(data)
    end
    def inspect
      "ValueRange<min=#{min.inspect},max=#{max.inspect}>"
    end
    def to_s
       range.to_s
    end
    def range
      Range.new(min,max)
    end
    def ValueRange.from_pml(_,data)
      ValueRange.new(data['min'],data['max'],data)
    end
    def to_pml
      { 'min' => @min, 'max' => @max }
    end
  end

  # value facts record results from value analysis (at the binary or bitcode level)
  #
  # program-point: pp
  # variable: mem-address-read, mem-address-write, register name
  # width: int
  # values: list of { min: x, max: x }
  class ValueFact < PMLObject
    attr_reader :attributes
    include ProgramInfoObject
    def initialize(programpoint, variable, width, values, attrs, data = nil)
      @programpoint, @variable, @width, @values = programpoint, variable, width, values
      @attributes = attrs
      set_yaml_repr(data)
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
      { 'program-point' => @programpoint.data,
        'variable' => @variable,
        'width' => @width,
        'values' => @values.data }.merge(attributes)
    end
    def to_s
      data.to_s
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
    def initialize(reference, cycles, wcetfreq, wcetcontrib, criticality=nil, data = nil)
      @reference, @cycles, @wcetfreq, @criticality = reference, cycles, wcetfreq, criticality
      set_yaml_repr(data)
    end
    def ProfileEntry.from_pml(fs, data)
      ProfileEntry.new(ContextRef.from_pml(fs,data['reference']), data['cycles'],
                       data['wcet_frequency'], data['wcet_contribution'], data['criticality'], data)
    end
    def criticality=(c)
      @criticality = c
      @data['criticality'] = c if @data
    end
    def to_pml
      { 'reference' => reference.data, 'cycles' => cycles, 'wcet_frequency' => wcetfreq,
        'criticality' => criticality, 'wcet_contribution' => wcet_contribution }.delete_if {|k,v| v.nil? }
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
      TimingEntry.new(Reference.from_pml(fs,data['scope']), data['cycles'],
                      profile,
                      ProgramInfoObject.attributes_from_pml(pml,data), data)
    end
    def to_pml
      pml = { 'scope' => @scope.data, 'cycles' => @cycles }.merge(attributes)
      pml['profile'] = @profile.data if @profile
      pml
    end
    def to_s
      "#<TimingEntry: scope=#{scope}, cycles=#{cycles} cycles,#{attributes.map{|k,v|"#{k}=#{v}"}.join(",")}>"
    end
  end


# end of module PML
end
