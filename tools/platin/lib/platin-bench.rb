#!/usr/bin/env ruby
#
require 'utils.rb'
include PML

require 'platin-analyze-trace'
require 'platin-extract-symbols'
require 'platin-pml2ais'
require 'platin-ait2pml'
require 'platin-transform'
require 'platin-wca'


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
    options.flow_fact_types = :supported
    options.flow_fact_srcs = ["trace"]
    options.timing_name = "platin-trace-all"
    step("Running WCA (machine code - trace facts)")     { WcaTool.run(pml, options) }
    options.flow_fact_types = FlowFact::MINIMAL_FLOWFACT_TYPES
    options.timing_name = "platin-trace-minimal"
    step("Running WCA (machine code - minimal trace facts)") { WcaTool.run(pml,options) }

    options.flow_fact_types = :supported
    options.flow_fact_srcs = "all"
    options.timing_name = "aiT-minimal"
    step("AIS Export")  {
      AisExportTool.run(pml,options)
      ApxExportTool.run(pml,options)
    }
    step("Run aiT")                        { AitAnalyzeTool.run(pml, options) }
    step("Import aiT Results")             { AitImportTool.run(pml,options) }
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
