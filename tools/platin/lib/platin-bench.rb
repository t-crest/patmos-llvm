#!/usr/bin/env ruby
#
require 'utils.rb'
include PML

require 'platin-analyze-trace'
require 'platin-extract-symbols'
require 'platin-wca'
require 'platin-pml2ais'
require 'platin-ait2pml'

class BenchTool
  TOOLS = [ExtractSymbolsTool,AnalyzeTraceTool,WcaTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool]

  def BenchTool.run(pml,options)
    ExtractSymbolsTool.run(options.binary,pml,options)
    AnalyzeTraceTool.run(options.binary,pml,options)
    WcaTool.run(pml,options)
    AisExportTool.run(pml,options.ais,options)
    ApxExportTool.run(pml,options)
    AitAnalyzeTool.run(pml, options)
    AitImportTool.run(pml,options)
    pml.data['timing'].each do |t|
      puts YAML::dump(t)
    end
    pml
  end
  def BenchTool.add_options(opts)
    TOOLS.each { |toolclass| toolclass.add_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract addresses, analyze trace, run aiT
EOF
  options, args = PML::optparse([:input], "program.elf.pml", SYNOPSIS) do |opts|
    BenchTool.add_options(opts)
  end
  BenchTool.run(PMLDoc.from_file(args[0]), options)
end
