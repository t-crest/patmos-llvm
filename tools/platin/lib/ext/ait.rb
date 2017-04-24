#
# The *platin* toolkit
#
# Bridge to absint's "aiT" WCET analyzer
#

require 'platin'
require "rexml/document"
require "rexml/formatters/transitive"

module PML

# option extensions for aiT
class OptionParser
  def apx_file(mandatory=true)
    self.on("-a", "--apx FILE", "APX file for a3") { |f| options.apx_file = f }
    self.add_check { |options| die_usage "Option --apx is mandatory" unless options.apx_file } if mandatory
  end
  def ais_file(mandatory=true)
    self.on("--ais FILE", "Path to AIS file") { |f| options.ais_file = f }
    self.add_check { |options| die_usage "Option --ais is mandatory" unless options.ais_file } if mandatory
  end
  def ait_report_prefix(mandatory=true)
    self.on("--ait-report-prefix PREFIX", "Path prefix for aiT's report and XML results") {
      |f| options.ait_report_prefix = f
    }
    self.add_check { |options| die_usage "Option --ait-report-prefix is mandatory" unless options.ait_report_prefix } if mandatory
  end
  def ait_icache_mode()
    self.on("--ait-icache-mode MODE", "aiT instruction cache analysis mode (normal|always-hit|always-miss|miss-if-unknown|hit-if-unknown)") {
      |f| options.ait_icache_mode = case f 
              when "normal" then "Normal"
	      when "always-miss" then "Always miss"
	      when "always-hit" then "Always hit"
	      when "miss-if-unknown" then "Miss if unknown"
	      when "hit-if-unknown" then "Hit if unknown"
	      else f
	  end
    }
  end
  def ait_dcache_mode()
    self.on("--ait-dcache-mode MODE", "aiT data cache analysis mode (normal|always-hit|always-miss|miss-if-unknown|hit-if-unknown)") {
      |f| options.ait_dcache_mode = case f 
              when "normal" then "Normal"
	      when "always-miss" then "Always miss"
	      when "always-hit" then "Always hit"
	      when "miss-if-unknown" then "Miss if unknown"
	      when "hit-if-unknown" then "Hit if unknown"
	      else f
	  end
    }
  end
  def ait_sca_type()
    # disable aiT's internal SC analysis (using a workaround) or enable different analysis methods using AIS annotations
    self.on("--ait-sca-type TYPE", "(internal(default)|anno-ptr|none)") { |t|
      options.ait_sca_type = case t
            when "internal" then nil # do nothing, the default
            when "none" # deactivate aiT's internal analysis
              options.ait_disable_internal_sc = true
              nil
            when "anno-ptr" # deactivate aiT's internal analysis, do analysis by tracking SC pointers
              options.ait_disable_internal_sc = true
              :anno_ptr
            else
              die_usage("unknown TYPE: #{t}")
            end
    }
  end
end

# Features not supported by the AIS/APX export module
class AISUnsupportedFeatureException < Exception
  def initialize(msg)
    super(msg)
  end
end

# Program Points not supported by the AIS/APX export module
class AISUnsupportedProgramPoint < AISUnsupportedFeatureException
  def initialize(pp, msg = "information references program point not supported by AIS exporter")
    super("#{msg} (#{pp} :: #{pp.class})")
    @pp = pp
  end
end


#
# Extend ValueRange with +#to_ais+
#
class ValueRange
  # Either the symbol this range references (double quoted), or a numeric range
  def to_ais
    if s = self.symbol
      dquote(s)
    else
      self.class.range_to_ais(range)
    end
  end

  def ValueRange.range_to_ais(range)
    sprintf("0x%08x .. 0x%08x",range.min,range.max)
  end
end

#
# Extend SymbolicExpression (and sublcassoes) with +#to_ais+
#
class SymbolicExpression
  def to_ais
    raise Exception.new("#{self.class}#to_ais: no translation available")
  end
end

class SEInt
  def to_ais ; self.to_s ; end
end

# Variables always reference arguments of functions
class SEVar
  def to_ais ; "@arg_#{self}" ; end
end

class SEBinary
  def to_ais
    left,right = self.a, self.b
    if SEBinary.commutative?(op) && left.constant? && ! right.constant?
      left, right = right, left
    end
    lexpr, rexpr = [left,right].map { |v| v.to_ais }
    # simplify (x * -1) to (-x)
    if right.constant? && right.constant == -1 && op == '*'
      return "-(#{lexpr})"
    end
    # simplify (a + -b) to (a - b)
    if right.constant? && right.constant < 0 && op == '+'
      return "#{lexpr} - #{-right}"
    end
    # translation of all other ops
    case op
    when '+'    then "(#{lexpr} + #{rexpr})"
    when '*'    then "(#{lexpr} * #{rexpr})"
    when '/u'   then "(#{lexpr} / #{rexpr})"
    when 'umax' then "max(#{lexpr},#{rexpr})"
    when 'umin' then "min(#{lexpr},#{rexpr})"
      # FIXME: how do we deal with signed variables?
    when 'smax' then "max(#{lexpr},#{rexpr})"
    when 'smin' then "min(#{lexpr},#{rexpr})"
    when '/s'   then "(#{lexpr} / #{rexpr})"
    else        raise Exception.new("SymbolicExpression#eval: unknown binary operator #{@op}")
    end
  end
end


#
# Extend program points with +#ais_ref+
#

class Function
  def ais_ref
    if self.label
      dquote(self.label)
    elsif self.address
      "0x#{address.to_s(16)}"
    else
      raise AISUnsupportedProgramPoint.new(self, "neither address nor label available (forgot 'platin extract-symbols'?)")
    end
  end
end

class Block
  def ais_ref
    if instructions.empty?
      raise AISUnsupportedProgramPoint.new(self, "impossible to reference an empty block")
    end
    if label
      dquote(label)
    elsif address
      "0x#{address.to_s(16)}"
    else
      raise AISUnsupportedProgramPoint.new(self, "neither address nor label available (forgot 'platin extract-symbols'?)")
    end
  end
end

class Instruction
  def ais_ref(opts = {})
    if address && block.label
      "#{block.ais_ref} #{opts[:ais2] ? "->":"+"} #{self.address - block.address} byte"
    elsif opts[:branch_index]
      assert("FIXME ais2 counts instructions from 0.") { opts[:ais2] == nil }
      "#{block.ais_ref} + #{opts[:branch_index]} branches"
    elsif address
      "0x#{address.to_s(16)}"
    else
      raise AISUnsupportedProgramPoint.new(self, "neither address nor symbolic offset available (forgot 'platin extract-symbols'?)")
      # FIXME: we first have to check whether our idea of instruction counting and aiT's match
      # "#{block.ais_ref} + #{self.index} instructions"
    end
  end
end

class Loop
  # no automatic translation for loops
  def ais_ref
    raise AISUnsupportedProgramPoint.new(self)
  end
end

class Edge
  def ais_ref
    raise AISUnsupportedProgramPoint.new(self)
  end
end

class ProgramPoint
  # generic helper that returns the address in hex
  def ais_addr
    # make sure we have an address (throw exception if not)
    assert("no ais_ref") { ais_ref }
    "0x#{address.to_s(16)}"
  end
end

# class to export PML information to AIS
class AISExporter

  MAX_METHODCACHE_ASSOCIATIVITY = Hash.new(8)
  MAX_METHODCACHE_ASSOCIATIVITY['LRU'] = 16

  attr_reader :stats_generated_facts,  :stats_skipped_flowfacts
  attr_reader :outfile, :options

  def initialize(pml,ais_file,options)
    @pml = pml
    @outfile = ais_file
    @options = options
    @entry = @pml.machine_functions.by_label(@options.analysis_entry)
    @extracted_arguments = {}
    @sc = {}
    @stats_generated_facts, @stats_skipped_flowfacts = 0, 0
  end

  # Generate a global AIS header
  def export_header

    # TODO get compiler type depending on YAML arch type
    @outfile.puts '# configure compiler'
    @outfile.puts 'compiler "patmos-llvm";'
    @outfile.puts ''

    export_machine_description

    if @options.ais_header_file
      @outfile.puts(File.read(@options.ais_header_file))
    end
    @outfile.puts
  end

  def export_machine_description
    @pml.arch.config.caches.each { |cache|
      case cache.name
      when 'data-cache'
        if cache.policy == "lru" or cache.policy == "fifo"
          gen_fact("cache data size=#{cache.size}, associativity=#{cache.associativity}, line-size=#{cache.line_size},"+
                   "policy=#{cache.policy.upcase}, may=chaos", "PML machine configuration")
        elsif cache.policy == "dm"
          gen_fact("cache data size=#{cache.size}, associativity=1, line-size=#{cache.line_size},"+
                   "policy=LRU, may=chaos", "PML machine configuration")
	elsif cache.policy == "ideal"
	  # TODO We can only configure an ideal cache by making the data memory zero-cycle accesses, which makes
	  # it different for bypasses! Check how the underlying memory is configured and die if the configuration
	  # is impossible to configure!
	  warn("aiT: found ideal data-cache. This requires the data memory to have zero-cycle access times.")
	elsif cache.policy != "no"
	  warn("aiT: unsupported data-cache policy #{cache.policy}, skipping data cache configuration")
	end
      when 'instruction-cache'
        gen_fact("cache code size=#{cache.size}, associativity=#{cache.associativity}, line-size=#{cache.line_size},"+
                 "policy=#{cache.policy.upcase}, may=chaos", "PML machine configuration")
      when 'method-cache' # new in aiT version >= 205838
        gen_fact("global method_cache_block_size=#{cache.block_size}","PML machine configuration")
        mcache_policy, mcache_assoc =  cache.policy.upcase, cache.associativity
        max_mcache_assoc = MAX_METHODCACHE_ASSOCIATIVITY[mcache_policy.upcase]
        if mcache_assoc > max_mcache_assoc
          warn("aiT: method cache with policy #{mcache_policy} does not support associativity > #{max_mcache_assoc}"+
               " (assuming associativity #{max_mcache_assoc})")
          mcache_assoc = max_mcache_assoc
        end
        line_size = 4 # template by absint
        cache_size = line_size * mcache_assoc
        gen_fact("cache code size=#{cache_size}, associativity=#{mcache_assoc}, line-size=#{line_size},"+
                 "policy=#{cache.policy.upcase}, may=chaos", "PML machine configuration")
      when 'stack-cache'
        # always enabled (new in aiT version >= 205838)
	# TODO check if the configuration requests a S$ or if the S$ is set to ideal and the memory is ideal, otherwise
	# die with an unsupported configuration error
      end
    }

    # work around aiT internal analysis by providing a 0-latency memory range for the stack cache accesses
    if @options.ait_disable_internal_sc
      @pml.arch.config.memory_areas.each { |area|
        if area.type == 'data' and area.address_range.max > 0xFFFF8000
          ar = area.address_range
          area.address_range = ValueRange.new(ar.min,0xFFFF7FFF,ar.symbol)
        end
      }
      sc_phantom = MemoryArea.new('sc_no_latency', 'data', nil,
                                  @pml.arch.config.memories.by_name('local'),
                                  ValueRange.new(0xFFFF8000,0xFFFFFFFF,nil))
      @pml.arch.config.memory_areas.add(sc_phantom)
      warn("aiT SC workaround, reserving phantom no-latency area #{sc_phantom.address_range.to_ais}")
    end


    @pml.arch.config.memory_areas.each { |area|
      kw = if area.type == 'code' then 'code' else 'data' end
      tt_read_first_beat = area.memory.read_latency + area.memory.read_transfer_time
      tt_write_first_beat = area.memory.write_latency + area.memory.write_transfer_time
      properties = [ "#{kw} read transfer-time = [#{tt_read_first_beat},#{area.memory.read_transfer_time}]" ]
      if area.cache
        # Changed in aiT version 205838 (should not be specified)
        #          properties.push("#{kw} cached")
      elsif area.type == 'scratchpad'
        properties.push("#{kw} locked")
      end
      if area.type != 'code'
        properties.push("#{kw} write time = #{tt_write_first_beat}")
      end
      address_range = area.address_range
      address_range = ValueRange.new(0,0xFFFFFFFF,nil) unless address_range
      transfer_bitsize = area.memory.transfer_size * 8
      gen_fact("area #{address_range.to_ais} features \"port_width\" = #{transfer_bitsize} and access #{properties.join(", ")}",
               "PML machine configuration")
    }
  end

  def gen_fact(ais_instr, descr, derived_from=nil)
    @stats_generated_facts += 1
    @outfile.puts(ais_instr+";" +" # "+descr)
    debug(@options,:ait) {
      s = " derived from #{derived_from}" if derived_from
      "Wrote AIS instruction: #{ais_instr}#{s}"
    }
    true
  end


  # Export jumptables for a function
  def export_jumptables(func)
    func.blocks.each do |mbb|
      branches = 0
      mbb.instructions.each do |ins|
        branches += 1 if ins.branch_type && ins.branch_type != "none"
        if ins.branch_type == 'indirect'
          successors = ins.branch_targets ? ins.branch_targets : mbb.successors
          targets = successors.uniq.map { |succ|
            succ.ais_ref
          }.join(", ")
          gen_fact("instruction #{ins.ais_ref(:branch_index => branches)} branches to #{targets}","jumptable (source: llvm)",ins)
        end
      end
    end
  end

  # export indirect calls
  def export_calltargets(ff, scope, callsite, targets)
    assert("Bad calltarget flowfact: #{ff.inspect}") { scope && scope.context.empty? }

    # no support for context-sensitive call targets
    unless callsite.context.empty?
      warn("aiT: no support for callcontext-sensitive callsites")
      return false
    end

    called = targets.map { |f| f.ais_ref }.join(", ")
    gen_fact("instruction #{callsite.programpoint.function.ais_ref} calls #{called}",
             "global indirect call targets (source: #{ff.origin})",ff)
  end

  # export loop bounds
  def export_loopbounds(scope, bounds_and_ffs)

    # context-sensitive facts not yet supported
    unless scope.context.empty?
      warn("aiT: no support for callcontext-sensitive loop bounds")
      return false
    end
    loopblock = scope.programpoint.loopheader

    origins = Set.new
    ais_bounds = bounds_and_ffs.map { |bound,ff|
      # (1) collect registers needed (and safe)
      # (2) generate symbolic expression
      origins.add(ff.origin)
      bound.referenced_vars.each { |v|
        user_reg = @extracted_arguments[ [loopblock.function,v] ]
        unless user_reg
          user_reg = "@arg_#{v}"
          @extracted_arguments[ [loopblock.function,v] ] = user_reg
          gen_fact("instruction #{loopblock.function.ais_ref} is entered with #{user_reg} = trace(reg #{v})",
                   "extracted argument for symbolic loop bound")
        end
      }
      bound.to_ais
    }.uniq
    bound = ais_bounds.length == 1 ? ais_bounds.first : "min(#{ais_bounds.join(",")})"

    # As we export loop header bounds, we should say the loop header is 'at the end'
    # of the loop (confirmed by absint (Gernot))
    loopname = dquote(loopblock.label)
    gen_fact("loop #{loopname} max #{bound} end",
             "global loop header bound (source: #{origins.to_a.join(", ")})")
  end

  # export global infeasibles
  def export_infeasible(ff, scope, pp)

    # context-sensitive facts not yet supported
    unless scope.context.empty? && pp.context.empty?
      warn("aiT: no support for context-sensitive scopes / program points: #{ff}")
      return false
    end

    # no support for empty basic blocks (typically at -O0)
    if pp.programpoint.block.instructions.empty?
      warn("aiT: no support for program points referencing empty blocks: #{ff}")
      return false
    end
    gen_fact("instruction #{pp.block.ais_ref} is never executed",
             "globally infeasible block (source: #{ff.origin})",ff)
  end

  def export_linear_constraint(ff)

    terms_lhs, terms_rhs = [],[]
    terms = ff.lhs.dup
    scope = ff.scope

    unless scope.context.empty?
      warn("aiT: no support for context-sensitive scopes: #{ff}")
      return false
    end

    # no support for context-sensitive linear constraints
    unless  terms.all? { |t| t.context.empty? }
      warn("aiT: no support for context-sensitive scopes / program points: #{ff}")
      return false
    end

    # we only export either (a) local flowfacts (b) flowfacts in the scope of the analysis entry
    type = :unsupported
    if ! scope.programpoint.kind_of?(Function)
      warn("aiT: linear constraint not in function scope (unsupported): #{ff}")
      return false
    end
    if scope.programpoint == @entry
      type = :global
    elsif ff.local?
      type = :local
    else
      warn("aiT: no support for interprocededural flow-facts not relative to analysis entry: #{ff}")
      return false
    end

    # no support for edges in aiT
    unless terms.all? { |t| t.programpoint.kind_of?(Block) }
      warn("Constraint not supported by aiT (not a block ref): #{ff}")
      return false
    end

    # no support for empty basic blocks (typically at -O0)
    if terms.any? { |t| t.programpoint.block.instructions.empty? }
      warn("Constraint not supported by aiT (empty basic block): #{ff})")
      return false
    end

    # Positivity constraints => do nothing
    rhs = ff.rhs.to_i
    if rhs >= 0 && terms.all? { |t| t.factor < 0 }
      return true
    end

    scope = scope.function.blocks.first
    terms.push(Term.new(scope,-rhs)) if rhs != 0
    terms.each { |t|
      set = (t.factor < 0) ? terms_rhs : terms_lhs
      set.push("#{t.factor.abs} (#{t.programpoint.block.ais_ref})")
    }
    cmp_op = "<="
    constr = [terms_lhs, terms_rhs].map { |set|
      set.empty? ? "0" : set.join(" + ")
    }.join(cmp_op)
    gen_fact("flow #{constr}",
             "linear constraint on block frequencies (source: #{ff.origin})",
             ff)
  end

  # export set of flow facts (minimum of loop bounds)
  def export_flowfacts(ffs)
    loop_bounds = {}
    ffs.each { |ff|
      if scope_bound = ff.get_loop_bound
        scope,bound = scope_bound
        next if options.ais_disable_export.include?('loop-bounds')
        next if ! bound.constant? && options.ais_disable_export.include?('symbolic-loop-bounds')
        (loop_bounds[scope]||=[]).push([bound,ff])
      else
        supported = export_flowfact(ff)
        @stats_skipped_flowfacts += 1 unless supported
      end
    }
    loop_bounds.each { |scope,bounds_and_ffs|
      export_loopbounds(scope, bounds_and_ffs)
    }
  end

  # export linear-constraint flow facts
  def export_flowfact(ff)
    assert("export_flowfact: loop bounds need to be exported separately") { ff.get_loop_bound.nil? }

    if (! ff.local?) && ff.scope.function != @entry
      warn("aiT: non-local flow fact in scope #{ff.scope} not supported")
      false

    elsif ff.symbolic_bound?
      debug(options, :ait) { "Symbolic Bounds only supported for loop bounds" }
      false

    elsif scope_cs_targets = ff.get_calltargets
      return false if options.ais_disable_export.include?('call-targets')
      export_calltargets(ff,*scope_cs_targets)

    elsif scope_pp = ff.get_block_infeasible
      return false if options.ais_disable_export.include?('infeasible-code')
      export_infeasible(ff,*scope_pp)

    elsif ff.blocks_constraint? || ff.scope.programpoint.kind_of?(Function)
      return false if options.ais_disable_export.include?('flow-constraints')
      export_linear_constraint(ff)

    else
      warn("aiT: unsupported flow fact type: #{ff}")
      false
    end
  end

  # export value facts
  def export_valuefact(vf)
    assert("AisExport#export_valuefact: programpoint is not an instruction (#{vf.programpoint.class})") { vf.programpoint.kind_of?(Instruction) }
    if ! vf.ppref.context.empty?
      warn("AisExport#export_valuefact: cannot export context-sensitive program point")
      return false
    end
    rangelist = vf.values.map { |v| v.to_ais }.join(", ")
    gen_fact("instruction #{vf.programpoint.ais_ref}" +
             " accesses #{rangelist}",
             "Memory address (source: #{vf.origin})", vf)
  end

  # export stack cache instruction annotation
  #def export_stack_cache_annotation(type, ins, value)
  #  assert("cannot annotate stack cache instruction w/o instruction addresses") { ins.address }
  #  if(type == :fill)
  #    feature = "stack_cache_fill_count"
  #  elsif(type == :spill)
  #    feature = "stack_cache_spill_count"
  #  else
  #    die("aiT: unknown stack cache annotation")
  #  end

  #  # XXX: the value is in bytes, aiT currently expects words (word-size blocks)
  #  assert("expected spill/fill value to be a multiple of word") { value % 4 == 0 }
  #  words = value / 4
  #  gen_fact("instruction #{ins.ais_ref} features \"#{feature}\" = #{words}", "SC words (source: llvm sca)")
  #end

  def add_stack_cache_inst(type, ins, value)
    assert("cannot annotate stack cache instruction w/o instruction addresses") { ins.address }
    assert("aiT: unknown stack cache annotation") {[:reserve, :free, :ensure].include? type}
    @sc[ins] = [type, value * 4]
  end

  def export_stack_cache_annotations
    return unless @options.ait_sca_type == :anno_ptr

    # no stack cache instructions to annotate
    return unless @sc.size

    unless sc_cache = @pml.arch.config.caches.find { |c| c.name == 'stack-cache' }
      warn("aiT: stack cache not configured, cannot add annotations")
      return
    end

    sz = sc_cache.size

    ais2 = AIS2Helper.new(@outfile)
    ais2.scope("routine", "#{@entry.ais_ref}")
    ais2.enter("enter")
    ais2.put("user(\"sc_top\") = 0x5000000,")
    ais2.put("user(\"m_top\")  = 0x5000000;")
    ais2.close
    ais2.close
    @sc.each { |instr,(type,n)|
      n_spill = "max(0, user(\"m_top\") - (user(\"sc_top\") - #{n}) - #{sz})"
      n_fill  = "max(0, #{n} - (user(\"m_top\") - user(\"sc_top\")))"
      ais2.scope("instruction", "#{instr.ais_ref(:ais2 => true)}")
      case type
      when :reserve
        ais2.enter("enter")
        ais2.put("user(\"n_spill\") = #{n_spill};")
        ais2.close
      when :ensure
        ais2.enter("enter")
        ais2.put("user(\"n_fill\") = #{n_fill};")
        ais2.close
      end

      ais2.enter("exit")
      case type
      when :reserve
        ais2.put("user(\"n_spill\") = (-inf..inf),")
        ais2.put("user(\"sc_top\") = user(\"sc_top\") - #{n},")
        ais2.put("user(\"m_top\") = user(\"m_top\") - #{n_spill};")
      when :free
        ais2.put("user(\"sc_top\") = user(\"sc_top\") + #{n},")
        ais2.put("user(\"m_top\") = max(user(\"sc_top\") + #{n}, user(\"m_top\"));")
      when :ensure
        ais2.put("user(\"n_fill\") = (-inf..inf),")
        ais2.put("user(\"m_top\") = user(\"m_top\") + #{n_fill};")
      end
      ais2.close_n(2)
    }
    @outfile.puts("}")

    # generate AIS1 instruction facts for spill/fill latencies
    @sc.each { |instr,(type,n)|
      data_area = @pml.arch.config.memory_areas.by_name('data')
      case type
      when :reserve
        gen_fact("instruction #{instr.ais_ref} additionally takes trace(@n_spill) / #{sc_cache.block_size} * "\
                 "#{data_area.memory.write_transfer_time} + #{data_area.memory.write_latency}  cycles", "spill cost")
      when :ensure
        gen_fact("instruction #{instr.ais_ref} additionally takes trace(@n_fill) / #{sc_cache.block_size} * "\
                 "#{data_area.memory.read_transfer_time} + #{data_area.memory.read_latency} cycles", "fill cost")
      end
    }
  end
end

class AIS2Helper
  def initialize(outfile)
    @outfile = outfile
    @stack = []
    put("ais2 {")
    push :scope
  end
  def push(s)
    @stack.push(s)
  end
  def pop
    @stack.pop
  end
  def put(str)
    @outfile.puts(' ' * 2 * @stack.size + str)
  end
  def scope(type, pp)
    put "#{type} #{pp} {"
    push :scope
  end
  def enter(enter_or_exit)
    put "#{enter_or_exit} with:"
    push :enter
  end
  def close
    s = pop
    put "}" if s == :scope
    s
  end
  def close_n (n)
    while(n > 0 && s = close)
      n -= 1
    end
  end

end

class APXExporter
  XMLNS_URL = "http://www.absint.com/apx"

  attr_reader :outfile

  def initialize(outfile, pml, options)
    @outfile, @pml, @options = outfile, pml, options
  end

  def export_project(binary, aisfile, report_prefix, analysis_entry)
    # There is probably a better way to do this .. e.g., use a template file.
    report  = report_prefix + ".txt"
    results = report_prefix + ".xml"
    report_analysis= report_prefix + ".#{analysis_entry}.xml"
    xmlns_url='http://www.absint.com/apx'
    xmlns="xmlns=\"#{xmlns_url}\""

    doc = REXML::Document.new "<!DOCTYPE APX><project></project>"
    project = doc.root
    project.add_attributes('xmlns' => xmlns_url, 'target' => @pml.arch.triple.first, 'version' => '13.04i')
    add_element(project, "options") { |proj_options|
      add_element(proj_options, "analyses_options") { |an_options|
        an_options << rexml_bool("extract_annotations_from_source_files", true)
        an_options << rexml_bool("xml_call_graph", true)
        an_options << rexml_bool("xml_show_per_context_info", true)
        an_options << rexml_bool("persistence_analysis", true)
        an_options << rexml_bool("xml_wcet_path", true)
        an_options << rexml_bool("xml_non_wcet_cycles", true)
        an_options << rexml_str("path_analysis_variant", "Prediction file based (ILP)")
	an_options << rexml_str("instruction_cache_mode", @options.ait_icache_mode) if @options.ait_icache_mode
	an_options << rexml_str("data_cache_mode", @options.ait_dcache_mode) if @options.ait_dcache_mode
      }
      add_element(proj_options, "general_options") { |gen_options|
        gen_options << rexml_str("include_path",".")
      }
      if arch_el = @pml.arch.config_for_apx(@options)
        proj_options << arch_el
      end
    }
    add_element(project, "files") { |files|
      [ ['executables', binary],  ['ais',aisfile],
        ['xml_results', results], ['report',report] ].each { |k,v|
        files << rexml_file(k,v)
      }
    }
    add_element(project, "analyses") { |analyses|
      add_element(analyses, "analysis") { |analysis|
        analysis.add_attributes('id' => "aiT", 'type' => 'wcet_analysis', 'enabled' => true)
        analysis << rexml_str("analysis_start", analysis_entry)
        analysis << rexml_file("xml_report", report_analysis)
      }
    }
    # Mandatory to use transitive formatter
    REXML::Formatters::Transitive.new( 2, false ).write(doc, @outfile)
  end

  private
  def add_element(parent, name)
    el = REXML::Element.new(name, parent)
    el.add_attributes('xmlns' => XMLNS_URL)
    yield el
    el
  end

  def rexml_bool(name, v)
    rexml_str(name, v ? 'true' : 'false')
  end

  def rexml_file(name, path)
    rexml_str(name, File.expand_path(path))
  end

  def rexml_str(name, v)
    el = REXML::Element.new(name)
    el << REXML::Text.new(v)
    el
  end
end


class CacheStats
  attr_reader :hits, :misses
  def initialize(hits, misses)
    @hits, @misses = hits, misses
  end
  def empty?
    @hits == 0 && @misses == 0
  end
end

class AitImport
  attr_reader :pml, :options
  def initialize(pml, options)
    @pml, @options = pml, options
    @routines = {}
    @blocks = {}
    @is_loopblock = {}
    @contexts = {}
  end
  def read_result_file(file)
    doc = REXML::Document.new(File.read(file))
    cycles = doc.elements["results/result[1]/cycles"].text.to_i
    scope = pml.machine_functions.by_label(options.analysis_entry)
    TimingEntry.new(scope,
                    cycles,
                    nil,
                    'level' => 'machinecode',
                    'origin' => options.timing_output)
  end
  def read_routines(analysis_elem)
    analysis_elem.each_element("decode/routines/routine") do |elem|
      address = Integer(elem.attributes['address'])
      routine = OpenStruct.new
      routine.instruction = pml.machine_functions.instruction_by_address(address)
      die("Could not find instruction at address #{address}") unless routine.instruction
      die("routine #{routine.instruction} is not a basic block") unless routine.instruction.block.instructions.first == routine.instruction
      if elem.attributes['loop']
        routine.loop = routine.instruction.block
        die("loop #{routine.loop} with id #{elem.attributes['id']} in loop routine #{routine.instruction} is not a loop header") unless routine.loop.loopheader?
      else
        routine.function = routine.instruction.function
        die("routine is not entry block") unless routine.function.entry_block == routine.instruction.block
      end
      routine.name = elem.attributes['name']
      @routines[elem.attributes['id']] = routine
      elem.each_element("block") { |be|
        @is_loopblock[be.attributes['id']] = true if elem.attributes['loop']
        unless be.attributes['address']
          debug(options,:ait) { "No address for block #{be}" }
          next
        end
        ins = pml.machine_functions.instruction_by_address(Integer(be.attributes['address']))
        @blocks[be.attributes['id']] = ins
      }
    end
    @routine_names = @routines.values.inject({}) { |memo,r| memo[r.name] = r; memo }
  end
  def read_contexts(contexts_element)
    contexts = {}
    contexts_element.each_element("context") { |elem|
      ctx = OpenStruct.new
      ctx.id = elem.attributes['id']
      ctx.routine = @routines[elem.attributes['routine']]
      if elem.text && elem.text != "no-history"
        ctx.context = elem.text.split(/\s*,\s*/).map { |s|
          callsite_addr, target = s.split("->",2)
          site = pml.machine_functions.instruction_by_address(Integer(callsite_addr))
          if target =~ /\A"(.*)"(?:\[(\d+)\/(\d+)(\.\.)?\])?\Z/
            routine = @routine_names[$1]
            if $2
              peel,last = $2.to_i, $3.to_i
              loopoffset = peel - 1
              loopstep = (peel == last && $3) ? 1 : 0
              LoopContextEntry.new(routine.loop, loopstep, loopoffset, site)
            else
              CallContextEntry.new(site)
            end
          else
            die("invalid contex target: #{target}")
          end
        }
      else
        ctx.context = []
      end
      if @contexts[ctx.id] && @contexts[ctx.id] != ctx
        raise Exception.new("Duplicate context with different meaning: #{ctx.id}")
      end
      @contexts[ctx.id] = Context.from_list(ctx.context)
    }
    contexts
  end
  #
  # read memory address ranges identified during value analysis
  #
  def read_value_analysis_results(analysis_task_elem)

    value_analysis_stats = Hash.new(0)

    facts = []
    fact_attrs = { 'level' => 'machinecode', 'origin' => 'aiT' }

    analysis_task_elem.each_element("value_analysis/value_accesses/value_access") { |e|

      ins = pml.machine_functions.instruction_by_address(Integer(e.attributes['address']))

      e.each_element("value_context") { |ce|

        context = @contexts[ce.attributes['context']]

        ce.each_element("value_step") { |se|

          # value_step#index ? value_step#mode?
          # value_area#mod? value_area#rem?

          fact_pp = ContextRef.new(ins, context)
          is_read = se.attributes['type'] == 'read'
          if is_read
            fact_variable = "mem-address-read"
          else
            fact_variable = "mem-address-write"
          end
          var_width = se.attributes['width'].to_i
          var_bitwidth = 32 # XXX: address width is always 32 for patmos

          unpredictable = false
          se.each_element("value_area") { |area|
            unpredictable = true if area.attributes['min'] != area.attributes['max']
          }
          debug(options,:ait) { "Access #{ins} in #{context}" } if unpredictable

          values = []
          se.each_element("value_area") { |area|
            min,max,rem,mod  = %w{min max rem mod}.map { |k|
              Integer(area.attributes[k]) if area.attributes[k]
            }
            # XXX: this is an aiT bug (probably because ranges are represented in a signed way)
            if min >= 0xffff_ffff_8000_000
              warn("Bad value range element #{min} - this should be fixed in recent aiT versions")
              min,max = [min,max].map { |v| v - 0xffff_ffff_0000_0000 }
            end
            debug(options,:ait) {
              sprintf("- %s 0x%08x..0x%08x (%d bytes), mod=0x%x rem=0x%x\n",
                      se.attributes['type'],min,max,max-min,mod || -1,rem || -1)
            } if unpredictable
            if(max < min)
              # aiT uses a wraparound domain in the new versions
              # see:
              #    Signedness-Agnostic Program Analysis
              #    Navas, Schachte, Sondergaard, Stuckey
              #    APLAS'12
              # We need to know the address width in order to
              # translate this to ordinary signed or unsigned intervals
              # For example, the wrap-around interval [8,4]
              # corresponds to the pair of unsigned intervals
              #  <tt> ( [0,4], [8,UINT_MAX] )</tt>
              # , and the pair of signed intervals
              #  <tt> ( [SIGNED_MIN,4], [8, SIGNED_MAX])
              # Both representations are ok with respect to PML semantics,
              # we go for the unsigned one for addresses.

              # if min < max+width, the information is not really useful
              if(min < max+var_width)
                values.push(ValueRange.new(0,2**var_bitwidth-1,nil))
              else
                values.push(ValueRange.new(0,max,nil))
                values.push(ValueRange.new(min,2**var_bitwidth-1,nil))
              end
            else
              values.push(ValueRange.new(min,max,nil))
            end
          }
          value_analysis_stats[(unpredictable ? 'un' : '') + 'predictable ' + (is_read ? 'reads' : 'writes')] += 1
          fact_values = ValueSet.new(values)
          facts.push(ValueFact.new(fact_pp, fact_variable, var_width, fact_values, fact_attrs.dup))
        }
      }
    }
    statistics("AIT",value_analysis_stats) if options.stats
    facts
  end

  # read the message lines with traces for n_spill and n_fill user regs
  def read_stack_cache_traces(analysis_task_elem)
    analysis_task_elem.each_element("value_analysis/messages/message/textline") { |e|
      if e.text =~ /trace.*\(context\w*(.*)\).*\("user_(n_(?:fill|spill))"\).=.(\d+)/
        yield $1,$2,$3
      end
    }
  end

  #
  # read results from WCET analysis
  #
  def read_wcet_analysis_results(wcet_elem, analysis_entry)
    read_contexts(wcet_elem.get_elements("contexts").first)

    @function_count, @function_cost = {} , Hash.new(0)

    # frequency of an edge on worst-case path
    edge_freq = {}

    # maximum number of cycles it takes to execute edge
    edge_cycles = {}

    # WCET contribution of an edge
    edge_contrib = {}

    ait_ins_cost, ait_ins_contrib = Hash.new(0), Hash.new(0)

    wcet_elem.each_element("wcet_path") { |e|
      # only read results for analysis entry
      rentry = e.get_elements("wcet_entry").first.attributes["routine"]
      entry = @routines[rentry]
      next unless  entry.function == analysis_entry

      # read results for each routine
      e.each_element("wcet_routine") { |re|
        # extract function cost
        routine = @routines[re.attributes['routine']]
        if routine.function
          @function_count[routine.function] = re.attributes['count'].to_i
          @function_cost[routine.function] += re.attributes['cumulative_cycles'].to_i
        else
          # loop cost
        end

        # extract edge cost (relative to LLVM terminology)
        re.each_element("wcet_context") { |ctx_elem|

          context = @contexts[ctx_elem.attributes['context']]

          # deal with aiT's special nodes
          start_nodes, loop_nodes, call_nodes, return_nodes = {}, {}, {}, {}

          # Special Case #1: (StartNode -> Node) is ignored
          # => If the target block is a start block, ignore the edge
          ctx_elem.each_element("wcet_start") { |elem|
            start_nodes[elem.attributes['block']] = true
          }

          # Special Case #2: (Node -> CallNode[Loop]) => (Node -> LoopHeaderNode)
          # => If we have an edge from a node to a 'loop call node', we need to replace
          #    it by an edge to the loop header node
          ctx_elem.each_element("wcet_edge_call") { |call_edge|
            if loopblock = @routines[call_edge.attributes['target_routine']].loop
              loop_nodes[call_edge.attributes['source_block']] = loopblock
            else
              call_nodes[call_edge.attributes['source_block']] = true
            end
          }

          # Special case #3: The unsolveable one?
          # In aiT we have edges from nodes within a loop to loop end nodes,
          # and edges from return nodes to nodes just after loop, but end nodes
          # and return nodes are not connected.
          # So in theory we could have the following situation in LLVM:
          #   a[L1] -> c v d v h[L1]
          #   b[L1] -> c v d v h[L1]
          # and in aiT:
          #   a[L1] -> end  ; return[L1] -> c
          #   b[L1] -> end  ; return[L1] -> d
          # and no sane way to determine the execution frequencies of a->c, a->d, b->c, b->d
          # If, however, all loop exit nodes x have a unique successor E(x) outside of the loop,
          # we simply increment the frequency of x->E(x) if we see x->EndOfLoop.
          # As I do not know of a better way to do it, we stuck with this strategy for now.
          ctx_elem.each_element("wcet_edge_return") { |return_edge|
            return_nodes[return_edge.attributes['target_block']] = true
          }

          # Now read in the cost for WCET edges
          ctx_elem.each_element("wcet_edge") { |edge|

            # skip if no cost associated with edge
            next unless edge.attributes['cycles'] || edge.attributes['path_cycles'] || edge.attributes['count']

            source_block_id = edge.attributes['source_block']

            # skip start nodes and return nodes
            next if start_nodes[source_block_id]
            next if return_nodes[source_block_id]

            source = @blocks[edge.attributes['source_block']]
            unless source
              warn("Could not find block corresponding to #{edge.attributes['source_block']}")
              next
            end

            target_block_id = edge.attributes['target_block']
            target = @blocks[edge.attributes['target_block']]

            # check if the target of this edge is in the same block as the source of the edge
            is_intrablock_target = ! target.nil? && target.index > 0 && ! loop_nodes[target_block_id]
            is_intrablock_target = true if call_nodes[target_block_id]

            # get the target block (if any), handling the case of a loop entry
            target_block = if loop_nodes[target_block_id]
                             loop_nodes[target_block_id]
                           else
                             b = @blocks[target_block_id]
                             b ? b.block : nil
                           end

            attr_count = edge.attributes['count'].to_i   # frequency on WCET path
            attr_cycles = edge.attributes['cycles'].to_i # WCET contribution
            attr_edge_cycles = edge.attributes['edge_cycles'].to_i # edge WCET

            # if there is no WCET for the edge, estimate it (WCET_contribution/WCET_frequency)
            if attr_count > 0
              computed_edge_cycles = (attr_cycles.to_f / attr_count).to_i
              if attr_edge_cycles.to_i == 0
                warn("No edge_cycles attribute for edge on WCET path (#{edge})")
                attr_edge_cycles = computed_edge_cycles
              end
            elsif attr_cycles > 0
              die("Bad aiT profile entry (contribution of edge not on WC path): #{edge}")
            end
            next unless attr_edge_cycles

            # We need to map the aiT edge to an LLVM edge
            #
            # In addition to the special cases discussed above,
            # there is the problem of duplicated blocks, as we want to compute cycles per execution,
            # not just cummulative cycles.
            #
            # One case is that we are given cycles for an intraprocedural edge b/i -> b/j (j>0), in different
            # contexts. As there might be several aiT nodes for b/i, we need store the maximum cost for
            # the slice (b/i..b/j) in this case. The frequency is ignored here, it is determined by
            # the frequency of the block anyway.
            # The other case is that we have cycles for an edge b/i -> b'/0; again there might be
            # several aiT nodes for b/i. In this case, we accumulate the frequency of (b -> b'),
            # and store the maximum cost for (b-> b').
            #
            # Later on, we add the maximum cost for every slice (b/i..b/j) to all edges
            # b->b' where b' is a live successor at instruction b/i.
            # Moreover, we add the maximum cost for (b/i -> b') to the edge b->b'.

            if source.block == target_block && is_intrablock_target
              ref = ContextRef.new(source, context)
              ait_ins_cost[ref]     = [ait_ins_cost[ref], attr_edge_cycles].max
              ait_ins_contrib[ref] += attr_cycles
              debug(options, :ait) { "Adding #{attr_cycles} cummulative cycles to #{ref} (intrablock)" }
            else
              pml_edge = source.block.edge_to(target_block ? target_block : nil)

              # handle the case where the target is an end-of-loop node, and we need to add the frequency
              # to the unique out-of-loop successor of the source. If it does not exist, we give up
              if ! target_block && @is_loopblock[target_block_id]
                exit_successor = nil

		loop do
                  source.block.successors.each { |s|
                    if source.block.exitedge_source?(s)
                      die("More than one exit edge from a block within a loop. This makes it" +
                          "impossible (for us) to determine correct edge frequencies") if exit_successor
                      exit_successor = s
                    end
                  }
		  break if exit_successor

		  # If we did not simplify the CFG, the exit edge might leave from a successor of the block. 
                  # Hence check if there is a single successor block with a single predecessor, continue the exit search
                  # from there.
		  if source.block.successors.length == 1 && source.block.successors.first.predecessors.length == 1
		    source = source.block.successors.first
		  else
                    die("no loop exit successor for #{source}, although there is an edge to end-of-loop node")
		  end
		end

                pml_edge = source.block.edge_to(exit_successor)
              end

              # store maximum cost for edge in context, and add to cummulative cost
              ref = ContextRef.new(pml_edge, context)
              ait_ins_cost[ref]     = [ait_ins_cost[ref], attr_edge_cycles].max
              ait_ins_contrib[ref] += attr_cycles
              debug(options, :ait) { "Adding #{attr_cycles} cummulative cycles to #{ref} (interblock)" }

              # if the edge is on the worst-case path, increase WCET frequency
              if attr_count > 0
                debug(options, :ait) { "Adding frequency to intraprocedural edge #{pml_edge}: #{attr_count} (#{edge})" }
                (edge_freq[pml_edge]||=Hash.new(0))[context] += attr_count
              end
            end
          }
        }
      }
    }

    # Now extract profile entries from the cost read from the aiT profile
    ait_ins_cost.each { |cref, path_cycles|
      contrib_cycles = ait_ins_contrib[cref]
      context = cref.context

      # if the edge cost is attributed to an instruction (intrablock cost),
      # add it to all edges that are live at this instruction
      # FIXME: WCET contribution is more difficult - we have to split it amongst
      # the outgoing edges..
      if cref.programpoint.kind_of?(Instruction)
        ins = cref.programpoint
        ins.block.outgoing_edges.each { |pml_edge|
          if ins.live_successor?(pml_edge.target)
            (edge_cycles[pml_edge]||=Hash.new(0))[context] += path_cycles
            (edge_contrib[pml_edge]||=Hash.new(0))[context] += contrib_cycles
          end
        }
      else
        pml_edge = cref.programpoint
        assert("read_wcet_analysis_result: expecting Edge type") { pml_edge.kind_of?(Edge) }

        (edge_cycles[pml_edge]||=Hash.new(0))[context]  += path_cycles
        (edge_contrib[pml_edge]||=Hash.new(0))[context] += contrib_cycles
      end
    }

    debug(options,:ait) { |&msgs|
      @function_count.each { |f,c|
        msgs.call "- function #{f}: #{@function_cost[f].to_f / c.to_f} * #{c}"
      }
    }

    # create profile entries
    profile_list = []
    edge_cycles.each { |e,ctxs|
      debug(options,:ait) { "- edge #{e}" }
      ctxs.each { |ctx,cycles|
        ref = ContextRef.new(e, ctx)
        freq = edge_freq[e] ? edge_freq[e][ctx] : 0
        contrib = freq * cycles # edge_contrib[e][ctx]
        debug(options,:ait) {
          sprintf(" -- %s: %d * %d (%d)\n",ctx.to_s, cycles, freq, contrib)
        }
        profile_list.push(ProfileEntry.new(ref, cycles, freq, contrib))
      }
    }
    profile_list
  end

  # Returns a map with [hits,misses] per cache-type
  # XXX  investigate scope of hit/miss stats
  def read_cache_stats(wcet_elem, analysis_entry)
    stats = {}
    wcet_elem.each_element("wcet_results/wcet_cache_infos/wcet_cache_info") { |e|
      # TODO check: can there be cache results for anything else than the analysis entry?
      #routine = @routines[e.attributes['routine']]
      type = e.attributes['type']
      stats[type] = CacheStats.new(e.attributes['hits'].to_i, e.attributes['misses'].to_i)
    }
    stats
  end

  def run
    analysis_entry  = pml.machine_functions.by_label(options.analysis_entry, true)
    timing_entry = read_result_file(options.ait_report_prefix + ".xml")

    ait_report_file = options.ait_report_prefix + ".#{options.analysis_entry}" + ".xml"
    analysis_task_elem = REXML::Document.new(File.read(ait_report_file)).get_elements("a3/wcet_analysis_task").first
    
    # read routines (but only if they are actually used later on)
    if options.import_block_timings || options.ait_import_addresses
      read_routines(analysis_task_elem)
      debug(options,:ait) { |&msgs|
	@routines.each do |id, r|
	  msgs.call("Routine #{id}: #{r}")
	end
      }
    end

    # read value analysis results
    read_contexts(analysis_task_elem.get_elements("value_analysis/contexts").first)
    if options.ait_import_addresses
      read_value_analysis_results(analysis_task_elem).each { |valuefact|
        pml.valuefacts.add(valuefact)
      }
    end

    read_stack_cache_traces(analysis_task_elem) { |ctx,sf,bytes|
      # bytes might be an interval e.g. "0..32"
      debug(options, :sca) { "#{ctx}: #{sf} = #{bytes} bytes" unless bytes == "0" }
    }

    # read wcet analysis results
    wcet_elem = analysis_task_elem.get_elements("wcet_analysis").first
    if options.import_block_timing
      timing_list = []
      read_wcet_analysis_results(wcet_elem, analysis_entry).each { |pe|
        timing_list.push(pe)
      }
      timing_entry.profile = Profile.new(timing_list)
    end

    # read cache statistics
    read_cache_stats(wcet_elem, analysis_entry).each { |t,v|
      timing_entry.attributes['cache-max-hits-' + t] = v.hits unless v.empty?
      timing_entry.attributes['cache-max-misses-' + t] = v.misses unless v.empty?
    }

    statistics("AIT","imported WCET results" => 1) if options.stats
    pml.timing.add(timing_entry)
  end
end

end # end module PML

