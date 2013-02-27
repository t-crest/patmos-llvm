#!/usr/bin/env ruby
#
require 'platin'
include PML

require 'platin/tools/analyze-trace'
require 'platin/tools/extract-symbols'
require 'platin/tools/pml2ais'
require 'platin/tools/ait2pml'
require 'platin/tools/transform'
require 'platin/tools/wca'

class BenchTool
  TOOLS = [ExtractSymbolsTool,AnalyzeTraceTool,
           WcaTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool]
  def BenchTool.step(descr)
    $stderr.puts("+ #{descr}")
    t1 = Time.now
    yield
    t2 = Time.now
    $stderr.puts("- #{descr.ljust(35)} #{((t2-t1)*1000).to_i} ms")
  end
  def BenchTool.run(pml,options)
    step("Sanity Checks") {
      rgs = pml.relation_graphs.select { |rg| rg.data['status'] != 'valid' && rg.src.name != "abort" }
      warn("Problematic Relation Graphs: #{rgs.map { |rg| "#{rg.qname} / #{rg.data['status']}" }.join(", ")}") unless rgs.empty?
    }
    step("Extracting Addresses")           { ExtractSymbolsTool.run(pml,options).dump_to_file(options.input) }
    step("Analyze MC Traces")              { AnalyzeTraceTool.run(pml,options).dump_to_file(options.input) } # XXX: during dev only

    ["all","minimal","local"].each do |selection|
      opts = options.dup
      opts.flow_fact_srcs = ["trace"]
      opts.flow_fact_selection = selection

      step("Running WCA (machine code - trace facts - #{selection})")  {
        opts.timing_name = "wca-trace-#{selection}"
        WcaTool.run(pml, opts)
      }
      step("Export aiT (machine code - trace facts - #{selection})")  {
        opts.timing_name = "aiT-trace-#{selection}"
        AisExportTool.run(pml,opts)
        ApxExportTool.run(pml,opts)
      }
      step("Run aiT (machine code - trace facts - #{selection})")  {
        opts.timing_name = "aiT-trace-#{selection}"
        AitAnalyzeTool.run(pml, options)
        AitImportTool.run(pml,options)
      }
    end
    step("Results: ") {
      pml.data['timing'].each do |t|
        puts YAML::dump(t)
      end
    }
    pml
  end
  def BenchTool.add_options(opts)
    opts.writes_pml
    TOOLS.each { |toolclass| toolclass.add_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT"
EOF
  options, args = PML::optparse([:input], "program.elf.pml", SYNOPSIS) do |opts|
    BenchTool.add_options(opts)
  end
  BenchTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
