#!/usr/bin/env ruby
#
require 'platin'
include PML

require 'tools/analyze-trace'
require 'tools/extract-symbols'
require 'tools/pml2ais'
require 'tools/ait2pml'
require 'tools/ff2pml'
require 'tools/transform'
require 'tools/wca'
require 'tools/sweet'
require 'tools/bench-trace'

class BenchToolSweet
  TOOLS = [ExtractSymbolsTool,
           AnalyzeTraceTool,
           WcaTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool,
           SweetAnalyzeTool, SweetImportTool ]

  def BenchToolSweet.step(descr, exit_on_error = false)
    BenchTool.step(descr, exit_on_error) { yield }
  end

  def BenchToolSweet.run(pml,options)
    needs_options(options, :outdir, :binary_file, :bitcode_file)
    mod = File.basename(options.binary_file, ".elf")

    # Configure intermediate files for aiT analysis
    options.ais_file, options.apx_file, options.ait_result_file, options.ait_report_file =
      %w{ais apx ait.xml ait.txt}.map { |ext| File.join(options.outdir, mod+"."+ext) }
    # Configure intermediate files for SWEET analysis
    options.alf_file, options.sweet_flowfact_file, options.sweet_trace_file =
      %w{alf ff tf}.map { |ext| File.join(options.outdir,mod+"."+ext) }

    step("Sanity Checks", true) {
      rgs = pml.relation_graphs.select { |rg| rg.data['status'] != 'valid' && rg.src.name != "abort" }
      warn("Problematic Relation Graphs: #{rgs.map { |rg| "#{rg.qname} / #{rg.data['status']}" }.join(", ")}") unless rgs.empty?
    }
    step("Extracting Addresses", true)           { ExtractSymbolsTool.run(pml,options).dump_to_file(options.input)  }

    step("Analyze MC Traces", true)              { AnalyzeTraceTool.run(pml,options) }

    step("Running SWEET analysis analysis and trace generation")  {
      opts = options.dup
      opts.sweet_ignore_volatiles = true
      opts.sweet_generate_trace = true
      SweetAnalyzeTool.run(pml, opts)
    }
    step("Import SWEET flow facts")        { SweetImportTool.run(pml, options) }

    step("Validate Relation Graph")        {
      begin
        RelationGraphValidationTool.run(pml,options)
      rescue Exception => detail
        $stderr.puts ("RG Validation failed: #{detail}")
      end
    }

    ["all", "local"].each do |selection|

      step("Running WCA (machine code - trace facts - #{selection})")  {
        opts = options.dup
        opts.flow_fact_selection = selection
        opts.timing_output = "wca-trace-#{selection}"
        opts.flow_fact_srcs = ["trace"]
        WcaTool.run(pml, opts)
      }

      step("Export/Analyze aiT (machine code - trace facts - #{selection})")  {
        opts = options.dup
        opts.flow_fact_selection = selection
        opts.timing_output = "aiT-trace-#{selection}"
        opts.flow_fact_srcs = ["trace"]
        AisExportTool.run(pml,opts)
        ApxExportTool.run(pml,opts)
        AitAnalyzeTool.run(pml, opts)
        AitImportTool.run(pml,opts)
      }

      step("Running WCA (relation graph - SWEET facts)")   {
        opts = options.dup
        opts.transform_action = "copy"
        opts.flow_fact_srcs   = ["trace"]
        opts.flow_fact_selection  = "rt-support-#{selection}"
        opts.flow_fact_output = "trace.support.#{selection}"
        RelationGraphTransformTool.run(pml,opts)

        opts.flow_fact_srcs = ["sweet","trace.support.#{selection}"]
        opts.flow_fact_selection = selection
        opts.use_relation_graph = true
        opts.timing_output = "wca-sweet-#{selection}"
        WcaTool.run(pml, opts)
      }
    end

    step("Results: ") {
      pml.timing.sort_by { |te|
        [ te.scope.qname, te.cycles, te.origin ]
      }.each { |te|
        info "#{te.scope} = #{te.cycles} (#{te.origin})"
      }
    } if options.stats || options.verbose
    pml
  end

  def BenchToolSweet.add_options(opts)
    opts.writes_pml
    opts.analysis_entry
    opts.binary_file(true)
    opts.bitcode_file(true)
    opts.on("--outdir DIR", "directory for generated files") { |d| opts.options.outdir = d}
    TOOLS.each { |toolclass| toolclass.add_config_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT"
EOF
  options, args = PML::optparse([:input], "program.elf.pml", SYNOPSIS) do |opts|
    BenchToolSweet.add_options(opts)
  end
  BenchToolSweet.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
