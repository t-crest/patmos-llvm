#!/usr/bin/env ruby
#
require 'utils.rb'
include PML

require 'platin-analyze-trace'
require 'platin-extract-symbols'
require 'platin-pml2ais'
require 'platin-ait2pml'
require 'platin-ff2pml'
require 'platin-transform'
require 'platin-wca'


class BenchToolSweet
  TOOLS = [SweetAnalyzeTool, SweetTraceTool, SweetImportTool,
           ExtractSymbolsTool,AnalyzeTraceTool, RelationGraphValidationTool,
           WcaTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool]
  def BenchToolSweet.step(descr)
    $stderr.puts("+ #{descr}")
    t1 = Time.now
    yield
    t2 = Time.now
    $stderr.puts("- #{descr.ljust(35)} #{((t2-t1)*1000).to_i} ms")
  end
  def BenchToolSweet.run(pml,options)
    step("Extracting Addresses")           { ExtractSymbolsTool.run(pml,options).dump_to_file(options.input) }
    step("Running SWEET analysis")         { SweetAnalyzeTool.run(pml, options) }
    step("Running SWEET trace generation") { SweetTraceTool.run(pml, options) }
    step("Import SWEET flow facts")        { SweetImportTool.run(pml, options) }
    step("Analyze MC Traces")              { AnalyzeTraceTool.run(pml,options) }
    step("Validate Relation Graph")        { RelationGraphValidationTool.run(pml,options) }
    step("Running WCA")                    { WcaTool.run(pml, options) }
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
  def BenchToolSweet.add_options(opts)
    TOOLS.each { |toolclass| toolclass.add_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT"
EOF
  options, args = PML::optparse([:input], "program.elf.pml", SYNOPSIS) do |opts|
    BenchToolSweet.add_options(opts)
  end
  BenchToolSweet.run(PMLDoc.from_file(options.input), options)
end
