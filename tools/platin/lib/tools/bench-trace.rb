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

class BenchTool
  TOOLS = [ExtractSymbolsTool,
           AnalyzeTraceTool,
           WcaTool,
           AisExportTool,ApxExportTool, AitAnalyzeTool,AitImportTool]

  def BenchTool.step(descr, exit_on_error = false)
    begin
      $stderr.puts("+ #{descr}")
      t1 = Time.now
      yield
      t2 = Time.now
      $stderr.puts("- #{descr.ljust(35)} #{((t2-t1)*1000).to_i} ms")
      return true
    rescue Exception => detail
      if exit_on_error
        $stderr.puts(detail.backtrace)
        die(detail.to_s)
      else
        warn("Analysis failed with exception: #{detail.to_s}")
        $stderr.puts detail.backtrace
        return false
      end
    end
  end

  def BenchTool.run(pml,options)

    diff = options.diff
    options.diff = false
    needs_options(options, :outdir, :binary_file)
    mod = File.basename(options.binary_file, ".elf")

    # Configure files for aiT export
    options.ais_file, options.apx_file, options.ait_result_file, options.ait_report_file =
      %w{ais apx ait.xml ait.txt}.map { |ext| File.join(options.outdir, mod+"."+ext) }

    # Sanity check and address extraction
    step("Sanity Checks", true) {
      rgs = pml.relation_graphs.select { |rg| rg.data['status'] != 'valid' && rg.src.name != "abort" }
      warn("Problematic Relation Graphs: #{rgs.map { |rg| "#{rg.qname} / #{rg.data['status']}" }.join(", ")}") unless rgs.empty?
    }
    step("Extracting Addresses", true)           { ExtractSymbolsTool.run(pml,options).dump_to_file(options.input)  }

    # Trace Analysis
    step("Analyze MC Traces", true)              { AnalyzeTraceTool.run(pml,options) }

    # WCET Analyses
    ["all","minimal","local"].each do |selection|

      step("Running WCA (machine code - trace facts - #{selection})")  {
        opts = options.dup
        opts.flow_fact_selection = selection
        opts.timing_output = "wca-trace-#{selection}"
        opts.flow_fact_srcs = ["trace"]
        opts.diff = diff
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
        opts.diff = diff
        AitImportTool.run(pml,opts)
      } unless options.disable_ait

      step("Copy trace flow facts which are cannnot covered by bitcode facts (compiler-rt)") {
        opts = options.dup
        opts.transform_action = "copy"
        opts.flow_fact_srcs   = ["trace"]
        opts.flow_fact_selection  = "rt-support-#{selection}"
        opts.flow_fact_output = "trace.support.#{selection}"
        RelationGraphTransformTool.run(pml,opts)
      }

      step("Running WCA-bitcode (bitcode - transformed trace facts - #{selection})") {
        opts = options.dup

        opts.flow_fact_srcs = ["trace"]
        opts.flow_fact_selection = selection
        opts.flow_fact_output = "trace.bitcode.#{selection}"
        opts.transform_action = "up"
        RelationGraphTransformTool.run(pml,opts)

        opts.flow_fact_srcs = ["trace.bitcode.#{selection}","trace.support.#{selection}"]
        opts.flow_fact_selection = "all" # already selected, and we only have global trafo atm
        opts.flow_fact_output = "trace.transformed.#{selection}"
        opts.transform_action = "down"
        RelationGraphTransformTool.run(pml,opts)

        opts.timing_output = "wca-uptrace-#{selection}"
        opts.flow_fact_selection = "all" # already selected and we only have global trafo atm
        opts.flow_fact_srcs = ["trace.transformed.#{selection}", "trace.support.#{selection}"]
        opts.diff = diff
        WcaTool.run(pml, opts)
      } # if selection != "minimal" # missing classification prevents to use minimal

    end
    # Summarize results
    step("Results: ") {
      pml.timing.sort_by { |te|
        [ te.scope.qname, te.cycles, te.origin ]
      }.each { |te|
        info "#{te.scope} = #{te.cycles} (#{te.origin})"
      }
    }
    pml
  end
  def BenchTool.add_options(opts)
    opts.writes_pml
    opts.analysis_entry
    opts.binary_file(true)
    opts.bitcode_file(false)
    opts.on("--outdir DIR", "directory for generated files") { |d| opts.options.outdir = d}
    opts.on("--disable-ait", "do not run aiT analysis") { |d| opts.options.disable_ait = true }
    TOOLS.each { |toolclass| toolclass.add_config_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT
EOF
  options, args = PML::optparse([], "", SYNOPSIS) do |opts|
    opts.needs_pml
    BenchTool.add_options(opts)
  end
  BenchTool.run(PMLDoc.from_files([options.input]), options).dump_to_file(options.output)
end
