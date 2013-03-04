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
  TOOLS = [ExtractSymbolsTool,AnalyzeTraceTool,
           WcaTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool]
  def BenchTool.step(descr, exit_on_error = false)
    begin
      $stderr.puts("+ #{descr}")
      t1 = Time.now
      yield
      t2 = Time.now
      $stderr.puts("- #{descr.ljust(35)} #{((t2-t1)*1000).to_i} ms")
    rescue Exception => detail
      if exit_on_error
        $stderr.puts(detail.backtrace)
        die(detail.to_s)
      else
        warn("Analysis failed: #{detail.to_s}")
      end
    end
  end
  def BenchTool.run(pml,options)
    step("Sanity Checks", true) {
      rgs = pml.relation_graphs.select { |rg| rg.data['status'] != 'valid' && rg.src.name != "abort" }
      warn("Problematic Relation Graphs: #{rgs.map { |rg| "#{rg.qname} / #{rg.data['status']}" }.join(", ")}") unless rgs.empty?
    }
    step("Extracting Addresses", true)           { ExtractSymbolsTool.run(pml,options).dump_to_file(options.input)  }
    step("Analyze MC Traces", true)              { AnalyzeTraceTool.run(pml,options) }

    ["all","minimal","local"].each do |selection|

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
        WcaTool.run(pml, opts)
      } # if selection != "minimal" # missing classification prevents to use minimal

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
  def BenchTool.add_options(opts)
    opts.writes_pml
    TOOLS.each { |toolclass| toolclass.add_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT
EOF
  options, args = PML::optparse([:input], "program.elf.pml", SYNOPSIS) do |opts|
    BenchTool.add_options(opts)
  end
  BenchTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
