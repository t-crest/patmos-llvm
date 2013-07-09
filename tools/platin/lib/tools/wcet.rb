#!/usr/bin/env ruby
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

#
# WCET Analysis command line tool
# Clients may subclass the WcetTool to implement benchmark drivers
#
class WcetTool
  TOOLS = [ExtractSymbolsTool,
           AnalyzeTraceTool,
           WcaTool,
           AlfTool, SweetAnalyzeTool, SweetImportTool,
           AisExportTool,ApxExportTool, AitAnalyzeTool,AitImportTool]
  attr_reader :pml, :options
  def initialize(pml, opts)
    @pml, @options = pml, opts.dup
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
    options.trace_analysis = true if options.use_trace_facts
    trace_analysis if options.trace_analysis
    sweet_analysis if options.enable_sweet

    transform_down(["llvm.bc"],"llvm")
    flow_srcs = ["llvm"]
    flow_srcs.push("trace") if options.use_trace_facts
    flow_srcs.push("sweet") if options.enable_sweet
    # FIXME: check if this is necessary (CFRG testsuite)
    flow_srcs.push("trace.support") if options.enable_sweet && options.trace_analysis

    wcet_analysis(flow_srcs)
    report
    pml
  end

  def prepare_pml
    # Sanity check and address extraction
    rgs = pml.relation_graphs.select { |rg| rg.data['status'] != 'valid' && rg.src.name != "abort" }
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
    wcet_analysis_platin(srcs) if options.enable_wca
    wcet_analysis_ait(srcs) unless options.disable_ait
  end

  def wcet_analysis_platin(srcs)
    time("run WCET analysis (platin)") do
      opts = options.dup
      opts.timing_output = [opts.timing_output,'platin'].compact.join("/")
      opts.flow_fact_selection ||= "all"
      opts.flow_fact_srcs = srcs
      WcaTool.run(pml, opts)
    end
  end

  def wcet_analysis_ait(srcs)
    time("run WCET analysis (aiT)") do
      # Simplify flow facts
      opts = options.dup
      opts.flow_fact_selection ||= "all"
      opts.flow_fact_srcs = srcs
      opts.flow_fact_output = [options.timing_output,'.aiT'].compact.join("/")
      opts.transform_action = 'simplify'
      opts.transform_eliminate_edges = true
      TransformTool.run(pml, opts)

      # run WCET analysis
      opts.flow_fact_selection = "all"
      opts.flow_fact_srcs = [ opts.flow_fact_output ]
      opts.timing_output = [options.timing_output,'aiT'].compact.join("/")
      AisExportTool.run(pml,opts)
      ApxExportTool.run(pml,opts)
      AitAnalyzeTool.run(pml, opts)
      AitImportTool.run(pml,opts)
      pml.flowfacts.reject! { |ff| ff.origin == '.aiT' }
    end
  end

  def report(additional_keys = [])
    results = summarize_results(additional_keys)
    file_open(options.report, (options.report_append ? "a" : "w")) do |fh|
      info "Writing report to #{options.report}" if options.report != "-"
      fh.puts YAML::dump(results.map { |r| r.merge(options.report_append || {}) })
    end if options.report
  end

  def summarize_results(additional_keys = [])
    trace_cycles = nil
    results = pml.timing.sort_by { |te|
      [ te.scope.qname, te.cycles, te.origin ]
    }.map { |te|
      trace_cycles = te.cycles if te.origin == "trace"
      dict = { 'analysis-entry' => options.analysis_entry,
        'source' => te.origin,
        'cycles' => te.cycles }
      additional_keys.each { |k| dict[k] = te[k] }
      dict
    }
    if options.runcheck
      die("wcet check: Not timing for simulator trace") unless trace_cycles
      pml.timing.each { |te|
        next if te.origin == "trace"
        if te.cycles + 1 < trace_cycles
          die("wcet check: cycles for #{te.origin} (#{te.cycles}) less than measurement (#{trace_cycles})")
        end
        if options.runcheck_factor && te.cycles > trace_cycles * options.runcheck_factor
          die <<-EOF
          WCET analysis check: Cycles for #{te.origin} (#{te.cycles}) larger than
          measurement (#{trace_cycles}) times #{options.runcheck_factor})
          EOF
        end
      }
    end
    results
  end

  def run_in_outdir
    begin
      outdir, tmpdir = options.outdir, nil
      tmpdir = outdir = Dir.mktmpdir() unless options.outdir
      mod = File.basename(options.binary_file, ".elf")

      configure_ait_files(outdir, mod, false) unless options.disable_ait

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
  def configure_ait_files(outdir, basename, overwrite = true)
    options.ais_file = File.join(outdir, "#{basename}.ais") unless (overwrite && options.ais_file)
    options.apx_file = File.join(outdir, "#{basename}.apx") unless (overwrite && options.apx_file)
    options.ait_result_file = File.join(outdir, "#{basename}.ait.xml") unless (overwrite && options.ait_result_file)
    options.ait_report_file = File.join(outdir, "#{basename}.ait.txt") unless (overwrite && options.ait_report_file)
  end

  def WcetTool.run(pml,options)
    # needs_options(:binary_file, :input)
    WcetTool.new(pml,options).run_in_outdir
  end

  def WcetTool.add_options(opts)
    opts.writes_pml
    opts.writes_report
    opts.analysis_entry
    opts.binary_file(true)
    opts.flow_fact_selection
    opts.calculates_wcet

    opts.on("--outdir DIR", "directory for generated files") { |d| opts.options.outdir = d }
    opts.on("--enable-trace-analysis", "run trace analysis") { |d| opts.options.trace_analysis = true }
    opts.on("--use-trace-facts", "use flow facts from trace") { |d| opts.options.use_trace_facts = true }
    opts.on("--disable-ait", "do not run aiT analysis") { |d| opts.options.disable_ait = true }
    opts.on("--enable-wca", "run platin WCA calculator") { |d| opts.options.enable_wca = true }

    opts.on("--enable-sweet", "run SWEET bitcode analyzer") { |d| opts.options.enable_sweet = true }
    use_sweet = Proc.new { |options| options.enable_sweet }
    opts.bitcode_file(use_sweet)
    opts.alf_file(Proc.new { false })
    opts.sweet_options
    opts.sweet_flowfact_file(Proc.new { false })

    opts.on("--check [FACTOR]", "check that analyzed WCET is higher than MOET [and less than MOET * FACTOR]") { |factor|
      opts.options.runcheck = true
      opts.options.runcheck_factor = factor.to_f
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
  updated_pml = WcetTool.run(PMLDoc.from_files(options.input), options)
  updated_pml.dump_to_file(options.output) if options.output
end
