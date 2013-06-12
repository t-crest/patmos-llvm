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

require 'tmpdir'

class WcetTool
  TOOLS = [ExtractSymbolsTool,
           AnalyzeTraceTool,
           WcaTool,
           AisExportTool,ApxExportTool, AitAnalyzeTool,AitImportTool]
  attr_reader :pml, :options
  def initialize(pml, opts)
    @pml, @options = pml, opts
  end
  def time(descr)
    begin
      t1 = Time.now
      yield
      t2 = Time.now
      info("Finished #{descr.ljust(35)} in #{((t2-t1)*1000).to_i} ms")
    end
  end

  def run
    # diff only for analyzers
    diff = options.diff
    options.diff = false
    # Sanity check and address extraction
    rgs = pml.relation_graphs.select { |rg| rg.data['status'] != 'valid' && rg.src.name != "abort" }
    warn("Problematic Relation Graphs: #{rgs.map { |rg| "#{rg.qname} / #{rg.data['status']}" }.join(", ")}") unless rgs.empty?
    # Extract Symbols
    time("read symbols") do
      ExtractSymbolsTool.run(pml,options)
    end
    # Trace Analysis
    time("analyze simulator trace") do
      opts = options.dup
      opts.flow_fact_output = options.use_trace_facts ? "trace" : ".trace.disabled" # XXX: should be an analyze-trace options
      opts.timing_output = "trace"
      unless opts.recorder_spec
        opts.recorder_spec = "g:cil,f:b:0"
        opts.recorder_spec += ",g:cil/0" if (opts.callstring_length || 0) > 0
      end
      AnalyzeTraceTool.run(pml,opts)
    end if options.trace_analysis || options.use_trace_facts
    # WCET Analysis
    time("run WCET analysis (platin)") do
      opts = options.dup
      opts.timing_output = "#{opts.timing_output}/platin"
      opts.flow_fact_selection = "all"
      opts.flow_fact_srcs = ["trace"]
      WcaTool.run(pml, opts)
    end unless options.disable_wca
    time("run WCET analysis (aiT)") do
      opts = options.dup
      opts.flow_fact_selection = "all"
      opts.timing_output = "#{opts.timing_output}/aiT"
      opts.flow_fact_srcs = ["trace"]
      AisExportTool.run(pml,opts)
      ApxExportTool.run(pml,opts)
      AitAnalyzeTool.run(pml, opts)
      AitImportTool.run(pml,opts)
    end unless options.disable_ait
    # Summarize results
    results = summarize_results
    file_open(options.report, "w") do |fh|
      info "Writing report to #{options.report}" if options.report != "-"
      fh.puts YAML::dump(results)
    end if options.report
    pml
  end

  def summarize_results
    trace_cycles = nil
    results = pml.timing.sort_by { |te|
      [ te.scope.qname, te.cycles, te.origin ]
    }.map { |te|
      trace_cycles = te.cycles if te.origin == "trace"
      { 'analysis-entry' => options.analysis_entry,
        'wcet-estimation' => te.origin,
        'cycles' => te.cycles }
    }
    if options.runcheck
      die("wcet check: Not timing for simulator trace") unless trace_cycles
      pml.timing.each { |te|
        next if te.origin == "trace"
        if te.cycles + 1 < trace_cycles
          die("wcet check: cycles for #{te.origin} (#{te.cycles}) less than measurement (#{trace_cycles})")
        end
        if options.runcheck_factor && te.cycles > trace_cycles * options.runcheck_factor
          die("wcet check: cycles for #{te.origin} (#{te.cycles}) greater than measurement (#{trace_cycles}) times #{options.runcheck_factor}")
        end
      }
    end
    results
  end

  def WcetTool.run(pml,options)
    # needs_options(:binary_file, :input)
    opts = options.dup
    begin
      if ! opts.outdir
        opts.outdir = tmpdir = Dir.mktmpdir()
      end
      # Configure files for aiT export
      mod = File.basename(opts.binary_file, ".elf")
      opts.ais_file, opts.apx_file, opts.ait_result_file, opts.ait_report_file =
        %w{ais apx ait.xml ait.txt}.map { |ext| File.join(opts.outdir, mod+"."+ext) }
      WcetTool.new(pml,opts).run # .dump_to_file(File.join(opts.outdir,mod+".pml"))
    ensure
      FileUtils.remove_entry tmpdir if tmpdir
    end
    pml
  end
  def WcetTool.add_options(opts)
    opts.writes_pml
    opts.writes_report
    opts.analysis_entry
    opts.binary_file(true)
    opts.bitcode_file(false)
    opts.calculates_wcet
    opts.on("--outdir DIR", "directory for generated files") { |d| opts.options.outdir = d }
    opts.on("--enable-trace-analysis", "run trace analysis") { |d| opts.options.trace_analysis = true }
    opts.on("--use-trace-facts", "use flow facts from trace") { |d| opts.options.use_trace_facts = true }
    opts.on("--disable-ait", "do not run aiT analysis") { |d| opts.options.disable_ait = true }
    opts.on("--disable-wca", "do not run WCA calculator") { |d| opts.options.disable_wca = true }
    opts.on("--check [FACTOR]", "check that analyzed WCET is higher than measured one and less than measuerd one time FACTOR (exit code)") { |factor|
      opts.options.runcheck = true
      opts.options.runcheck_factor = factor.to_f
    }
    TOOLS.each { |toolclass| toolclass.add_config_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT
EOF
  options, args = PML::optparse([], "", SYNOPSIS) do |opts|
    opts.needs_pml
    WcetTool.add_options(opts)
  end
  updated_pml = WcetTool.run(PMLDoc.from_files(options.input), options)
  updated_pml.dump_to_file(options.output) if options.output
end
