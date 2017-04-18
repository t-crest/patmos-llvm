#
# platin toolkit
#
require 'platin'
include PML

require 'tools/analyze-trace'
require 'tools/extract-symbols'
require 'tools/pml2ais'
require 'tools/ait2pml'
require 'tools/transform'
require 'tools/wca'
require 'tools/ff2pml'
require 'tools/sweet'
require 'tmpdir'

# High-Level Wrapper for aiT
# XXX: Internal tool; move into different directory; these tools are not visible on the command line)
class AitTool
  def AitTool.run(pml,opts)
    AisExportTool.run(pml,opts)
    ApxExportTool.run(pml,opts)
    AitAnalyzeTool.run(pml, opts)
    AitImportTool.run(pml,opts)
    if opts.verbose
      timing = pml.timing.by_origin(opts.timing_output).last
      puts "Cycles: #{timing.cycles}"
      puts "Edge Profile:"
      timing.profile.each { |pe|
        next unless pe.wcetfreq > 0
        puts "  #{pe.reference}: #{pe.wcetfreq} (#{pe.wcetfreq * pe.cycles} cyc)"
      }
    end
  end
  def AitTool.add_config_options(opts)
    AisExportTool.add_config_options(opts)
    ApxExportTool.add_config_options(opts)
    AitAnalyzeTool.add_config_options( opts)
    AitImportTool.add_config_options(opts)
  end
end

# number of overestimated cycles always tolerated
CHECK_OVERESTIMATION_TOLERANCE=10

#
# WCET Analysis command line tool
# Clients may subclass the WcetTool to implement benchmark drivers
#
class WcetTool
  attr_reader :additional_report_info

  TOOLS = [ ExtractSymbolsTool,
            AnalyzeTraceTool,
            WcaTool,
            AitTool,
            AlfTool, SweetAnalyzeTool, SweetImportTool ]
  attr_reader :pml, :options
  def initialize(pml, opts)
    @pml, @options = pml, opts.dup
    @additional_report_info = {}
  end

  def analysis_entry
    pml.machine_functions.by_label(options.analysis_entry)
  end

  def time(descr)
    begin
      t1 = Time.now
      yield
      t2 = Time.now
      info("Finished #{descr.ljust(35)} in #{((t2-t1)*1000).to_i} ms")
    end
  end

  # replace this method in a benchmark subclass
  def run_analysis
    prepare_pml
    unless analysis_entry
      die("Analysis entry '#{options.analysis_entry}' not found (check for typos, inlined functions or code not reachable from program entry)")
    end
    options.use_trace_facts = true if options.compare_trace_facts
    options.trace_analysis = true if options.use_trace_facts
    trace_analysis if options.trace_analysis
    sweet_analysis if options.enable_sweet
    transform_down(["llvm.bc"],"llvm")
    transform_down(["user.bc"],"user")

    flow_srcs = ["llvm", "user"]
    wcet_analysis(flow_srcs) if options.compare_trace_facts

    flow_srcs.push("trace") if options.use_trace_facts
    flow_srcs.push("sweet") if options.enable_sweet

    # FIXME: check if this is necessary (CFRG testsuite)
    flow_srcs.push("trace.support") if options.enable_sweet && options.trace_analysis

    # TODO we should (also?) add a 'configuration' name to the TimingEntry that refers to an analysis-configuration name
    options.timing_output = [options.timing_output,'trace'].compact.join('/') if options.use_trace_facts

    wcet_analysis(flow_srcs)
    report(additional_report_info)
    pml
  end

  def prepare_pml
    # Sanity check and address extraction
    rgs = pml.relation_graphs.list.select { |rg| rg.data['status'] != 'valid' && rg.src.name != "abort" }
    warn("Problematic Relation Graphs: #{rgs.map { |rg| "#{rg.qname} / #{rg.data['status']}" }.join(", ")}") unless rgs.empty?

    # Extract Symbols
    time("read symbols") do
      ExtractSymbolsTool.run(pml,options)
    end
  end

  def trace_analysis
    time("analyze simulator trace") do
      opts = options.dup
      opts.flow_fact_output = "trace"
      opts.timing_output = [opts.timing_output,"trace"].compact.join("/")
      unless opts.recorder_spec
        opts.recorder_spec = "g:cil,f:b:0"
        opts.recorder_spec += ",g:cil/0" if (opts.callstring_length || 0) > 0
      end
      AnalyzeTraceTool.run(pml,opts)

      # copy machine-code facts necessary for bitcode analysis to trace.support
      opts.transform_action = "copy"
      opts.flow_fact_srcs = ["llvm","trace"]
      opts.flow_fact_selection = "rt-support-#{options.flow_fact_selection}"
      opts.flow_fact_output = "trace.support"
      TransformTool.run(pml, opts)
    end
  end

  def sweet_analysis
    time("SWEET analysis") do
      opts = options.dup
      opts.flow_fact_output = "sweet.bc"
      SweetAnalyzeTool.run(pml, opts)
      SweetImportTool.run(pml, opts)

      # transform SWEET flow facts to machine code
      opts.transform_action = "down"
      opts.flow_fact_srcs = ["sweet.bc","trace.support"]
      opts.flow_fact_selection = "all"
      opts.flow_fact_output = "sweet"
      TransformTool.run(pml,opts)
    end
  end

  def transform_down(srcs, output)
    time("Flow Fact Transformation #{srcs}") do
      opts = options.dup
      opts.flow_fact_selection ||= "all"
      opts.flow_fact_srcs = srcs
      opts.flow_fact_output = output
      opts.transform_action = 'down'
      TransformTool.run(pml, opts)
    end
  end

  def wcet_analysis(srcs)
    run_wca = options.enable_wca
    if options.combine_wca
      # TODO is there a way to disable the stack cache analysis in aiT as well and use the platin analysis??
      options.ait_icache_mode = "always-hit" 
      run_wca = true
    end
    begin
      wcet_analysis_ait(srcs) unless options.disable_ait
    rescue Exception => ex
      $stderr.puts ex.backtrace
      # don't fall back to internal WCET analysis if disabled explicitly
      return if options.enable_wca == false
      warn("a3 WCET analysis failed: #{ex}. Trying platin WCET analysis.")
      run_wca = true
      options.combine_wca = false
    end
    wcet_analysis_platin(srcs) if run_wca
  end

  def wcet_analysis_platin(srcs)
    time("run WCET analysis (platin)") do
      opts = options.dup
      opts.import_block_timing = true if opts.compute_criticalities
      opts.timing_output = [opts.timing_output,'platin'].compact.join("/")
      opts.flow_fact_selection ||= "all"
      opts.flow_fact_srcs = srcs
      if opts.combine_wca
        # We only need the I$ analysis in this mode, everything else *must* be disabled
        opts.ignore_instruction_timing = true
	opts.disable_sca = true
	opts.disable_dca = true
      end
      WcaTool.run(pml, opts)
      compute_criticalities(opts) { |pml,tmp_opts,src,round|
        tmp_opts.flow_fact_srcs.push(src)
        WcaTool.run(pml,tmp_opts)
      } if opts.compute_criticalities
    end
  end

  def wcet_analysis_ait(srcs)
    time("run WCET analysis (aiT)") do
      pml.with_temporary_sections([:flowfacts]) do

        # Simplify flow facts
        simplify_flowfacts(srcs, options)
        simplified_sources =  srcs.map { |src| src + ".simplified" }

        # run WCET analysis
        opts = options.dup
        opts.flow_fact_selection = "all"
        opts.import_block_timing = true if opts.compute_criticalities
        opts.flow_fact_srcs = simplified_sources
        opts.timing_output = [options.timing_output,'aiT'].compact.join("/")
        # Note: for opts.combined_wca mode we already disabled the I$ analsis earlier, everything else is used (for now)
        AitTool.run(pml,opts)

        # criticality analysis
        compute_criticalities(opts) { |pml,tmp_opts,src,round|
          simplify_flowfacts([src], tmp_opts)
          tmp_opts.flow_fact_srcs.push(src+".simplified")
          configure_ait_files(tmp_opts, File.dirname(options.ait_report_prefix), "criticality", true)
          AitTool.run(pml,tmp_opts)
        } if opts.compute_criticalities
      end
    end
  end

  def simplify_flowfacts(srcs, opts)
    opts = opts.dup
    opts.flow_fact_selection ||= "all"
    srcs.each { |src|
      opts.flow_fact_srcs    = [src]
      opts.flow_fact_output = src + ".simplified"
      opts.transform_action = 'simplify'
      opts.transform_eliminate_edges = true
      TransformTool.run(pml, opts)
    }
  end

  def compute_criticalities(opts)
    opts = opts.dup
    criticality = {}
    missing_blocks, missing_edges = Set.new, Set.new
    pml.machine_functions.each { |f|
      f.blocks.each { |b| missing_blocks.add(b) }
      f.edges.each { |e| missing_edges.add(e) }
    }
    timing = pml.timing.find { |t| t.origin == opts.timing_output }
    cycles = timing.cycles
    wcet_cycles = timing.cycles
    round, found_new_edge = 0, true
    while true
      info("Criticality Iteration #{round+=1}: #{cycles} (blockmode=#{! missing_blocks.nil?})")
      if cycles < 0
        if missing_blocks
          missing_blocks = nil
        else
          debug(opts,:wcet) { "compute_criticalities: no more feasible edges" }
          break
        end
      else
        found_new_edge = false
        timing.profile.each { |t|
          next unless t.wcetfreq > 0
          unless criticality[t.reference.programpoint]
            criticality[t.reference.programpoint] = cycles
            missing_blocks.delete(t.reference.programpoint.source) if missing_blocks
            missing_edges.delete(t.reference.programpoint)
            found_new_edge = true
          end
        }
        if missing_edges.empty?
          debug(opts,:wcet) { "compute_criticalities: 100% edge coverage" }
          break
        end
        unless found_new_edge
          if missing_blocks
            missing_blocks = nil
          else
            warn("compute_criticalities: Feasible problem, so we should have detected new edges on WCET path")
            break
          end
        end
      end
      ff = enforce_blocks_constraint(missing_blocks ? missing_blocks : missing_edges, '.criticality')
      tmp_opts = opts.dup
      pml.with_temporary_sections([:flowfacts,:timing]) do
        debug(opts,:wcet) { "Adding constraint to enforce different WCET path: #{ff}" }
        pml.flowfacts.push(ff)
        pml.timing.clear!
        tmp_opts.disable_ipet_diagnosis = true
        tmp_opts.stats = false
        begin
          yield pml,tmp_opts,'.criticality',round
          timing = pml.timing.find { |t| t.origin == opts.timing_output }
          cycles = timing.cycles
        rescue InconsistentConstraintException => ex # Inconsistent problem
          cycles = -1
        end
      end
    end

    # done, report
    missing_edges.each { |e| criticality[e] = 0 }
    debug(options, :wcet) { |&msgs| criticality.each { |k,v| msgs.call("#{k}: #{v.to_f / wcet_cycles}") } }

    # TODO: create context-free profile, unless available
    timing = pml.timing.find { |t| t.origin == opts.timing_output }

    criticality.each { |ref,v|
      ref = ContextRef.new(ref,Context.empty)
      crit = v.to_f / wcet_cycles
      pe = timing.profile.by_reference(ref)
      unless pe
        pe = ProfileEntry.new(ref, nil, nil, nil, crit)
        timing.profile.add(pe)
      end
      pe.criticality = crit
    }
  end

  def enforce_blocks_constraint(edges_or_blocks, origin)
    attrs = { 'level' => 'machinecode', 'origin' => origin }
    scoperef = analysis_entry
    terms = edges_or_blocks.map { |ppref|
      Term.new(ppref, -1)
    }
    FlowFact.new(scoperef, TermList.new(terms), 'less-equal', -1, attrs)
  end


  def report(additional_info = {})
    results = summarize_results(additional_info)
    file_open(options.report, (options.report_append ? "a" : "w")) do |fh|
      info "Writing report to #{options.report}" if options.report != "-"
      fh.puts YAML::dump(results.map { |r| r.merge(options.report_append || {}) })
    end if options.report
  end

  def summarize_results(additional_info = {})
    trace_cycles = nil
    wcet_cycles = nil
    combined_cycles = 0

    results = pml.timing.sort_by { |te|
      [ te.scope.qname, te.cycles, te.origin ]
    }.map { |te|
      trace_cycles = te.cycles if te.origin == "trace"
      wcet_cycles = [wcet_cycles,te.cycles].compact.min if te.origin != "trace"
      combined_cycles += case te.origin
	  when "aiT"    then te.cycles
	  when "platin" then te.attributes['cache-cycles-instr'] || 0
	  else 0
      end
      dict = { 'analysis-entry' => options.analysis_entry,
        'source' => te.origin,
        'cycles' => te.cycles }
      te.attributes.select { |k,v| k.start_with? 'memory-' }.each { |k,v| dict[k] = v }
      te.attributes.select { |k,v| k.start_with? 'cache-' }.each { |k,v| dict[k] = v }
      te.attributes.select { |k,v| k.start_with? 'cycles-' }.each { |k,v| dict[k] = v }
      (additional_info[te.origin] || []).each { |k,v| dict[k] = v }
      dict
    }
    if options.combine_wca
      wcet_cycles = combined_cycles
      results.push( { 'analysis-entry' => options.analysis_entry, 
                      'source' => 'combined', 
		      'cycles' => combined_cycles 
		    } )
    end
    if options.runcheck and not trace_cycles.nil?
      die("wcet check: No timing for simulator trace") unless  trace_cycles > 0
      die("wcet check: No WCET results") unless wcet_cycles and wcet_cycles > 0
      pml.timing.each { |te|
        next if te.origin == "trace"
	next if te.origin != "combined" and options.combine_wca
        next unless te.cycles >= 0
	# TODO remember the trace_cycles per analysis-entry, check depending on analysis-entry
        if te.cycles < trace_cycles
          die("wcet check: cycles for #{te.origin} (#{te.cycles}) less than measurement (#{trace_cycles})")
        end
        if options.runcheck_factor.to_f > 0.0
          tolerated_overestimation = (trace_cycles * options.runcheck_factor.to_f) + CHECK_OVERESTIMATION_TOLERANCE
          if te.cycles > tolerated_overestimation
            die <<-EOF.strip_heredoc.delete("\n")
              WCET analysis check: Cycles for #{te.origin} (#{te.cycles}) #{te.cycles.fdiv(trace_cycles).round(2)} 
	      times larger than measurement (#{trace_cycles})
            EOF
          end
        end
      }
    end
    info "#{"Trace analysis: #{trace_cycles} cycles; " if trace_cycles}best WCET bound: #{wcet_cycles} cycles"
    results
  end

  def run_in_outdir
    begin
      outdir, tmpdir = options.outdir, nil
      tmpdir = outdir = options.outdir = Dir.mktmpdir() unless options.outdir
      mod = File.basename(options.binary_file, ".elf")

      configure_ait_files(options, outdir, mod, false) unless options.disable_ait

      if options.enable_sweet
        options.alf_file = File.join(outdir, mod+".alf") unless options.alf_file
        options.sweet_flowfact_file = File.join(outdir, mod+".ff") unless options.sweet_flowfact_file
        options.sweet_trace_file = File.join(outdir, mod+".tf") unless options.sweet_trace_file
      end
      run_analysis
    ensure
      FileUtils.remove_entry tmpdir if tmpdir
    end
    pml
  end

  # Configure files for aiT export
  def configure_ait_files(opts, outdir, basename, overwrite = true)
    opts.ais_file = File.join(outdir, "#{basename}.ais") unless (!overwrite && opts.ais_file)
    opts.apx_file = File.join(outdir, "#{basename}.apx") unless (!overwrite && opts.apx_file)
    opts.ait_report_prefix = File.join(outdir, "#{basename}.ait") unless (!overwrite && opts.ait_report_prefix)
  end

  def WcetTool.run(pml,options)
    needs_options(:input)
    
    # Get analysis configurations from PML
    # TODO Add option to optionally specify config section name
    # TODO Support running multiple analysis configurations? Would be more efficient to let the actual analysis tool handle this
    #      so analysis steps that are shared between configurations are run only once (like address extraction or trace analysis).
    config = pml.analysis_configurations.by_name('default')
    if config and not options.analysis_entry
      options.analysis_entry = config.analysis_entry
    end
    if not options.analysis_entry
      warn("Analysis entry not specified, falling back to 'main'.") if config
      options.analysis_entry = "main"
    end

    WcetTool.new(pml,options).run_in_outdir
  end

  def WcetTool.add_options(opts)
    opts.writes_pml
    opts.writes_report
    opts.analysis_entry(false)
    opts.binary_file(true)
    opts.flow_fact_selection
    opts.accept_corrected_rgs
    opts.calculates_wcet
    opts.on("--batch", "run in batch processing mode, reading analysis targets and configuration from PML file") { opts.options.batch = true }
    opts.on("--outdir DIR", "directory for generated files") { |d| opts.options.outdir = d }
    opts.on("--enable-trace-analysis", "run trace analysis") { |d| opts.options.trace_analysis = true }
    opts.on("--use-trace-facts", "use flow facts from trace") { |d| opts.options.use_trace_facts = true }
    opts.on("--compare-trace-facts", "run WCET analysis with and without trace facts") { |d| opts.options.compare_trace_facts = true }
    opts.on("--disable-ait", "do not run aiT analysis") { |d| opts.options.disable_ait = true }
    opts.on("--[no-]enable-wca", "run platin WCA calculator") { |b| opts.options.enable_wca = b }
    opts.on("--combine-wca", "run both aiT and WCA and combine cache analysis results") { |d| opts.options.combine_wca = true }
    opts.on("--compute-criticalities", "calculate block criticalities") { opts.options.compute_criticalities = true }
    opts.on("--enable-sweet", "run SWEET bitcode analyzer") { |d| opts.options.enable_sweet = true }
    use_sweet = Proc.new { |options| options.enable_sweet }
    opts.bitcode_file(use_sweet)
    opts.alf_file(Proc.new { false })
    opts.sweet_options
    opts.sweet_flowfact_file(Proc.new { false })
    opts.on("--check [FACTOR]", "check that analyzed WCET is higher than MOET [and less than MOET * FACTOR]") { |factor|
      opts.options.runcheck = true
      opts.options.runcheck_factor = factor.to_f # defaults to nil.to_f (=0.0)
    }
    TOOLS.each { |toolclass| toolclass.add_config_options(opts) }
  end
end

if __FILE__ == $0
  synopsis=<<EOF
platin WCET tool
EOF
  options, args = PML::optparse([], "", synopsis) do |opts|
    opts.needs_pml
    WcetTool.add_options(opts)
  end
  unless which(options.a3)
    warn("Commercial a3 tools is not available; use --disable-ait to hide this warning") unless options.disable_ait
    options.disable_ait = true
    options.enable_wca = true
    options.combine_wca = false
  end
  if options.combine_wca and options.disable_ait
    warn("Use of a3 has been disabled, combined WCET analysis is not available")
    options.combine_wca = false
    options.enable_wca = true
  end
  if options.combine_wca and options.compute_criticalities
    # We could still do it using aiT, but it would be rather imprecise
    die("Computing criticalities is not possible in combined-WCA mode")
  end

  updated_pml = WcetTool.run(PMLDoc.from_files(options.input), options)
  updated_pml.dump_to_file(options.output) if options.output
end
