#!/usr/bin/env ruby
#
require 'utils.rb'
include PML

require 'psk-analyze-trace'
require 'psk-extract-symbols'
require 'psk-pml2ais'
require 'psk-ait2pml'
require 'psk-ff2pml'
require 'psk-wca'

class BenchToolSweet
  TOOLS = [SweetAnalyzeTool,SweetImportTool,
           ExtractSymbolsTool,AnalyzeTraceTool, WcaTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool]

  def BenchToolSweet.run(pml,options)
    SweetAnalyzeTool.run(pml, options)
    SweetImportTool.run(pml, options)
    ExtractSymbolsTool.run(pml,options)
    AnalyzeTraceTool.run(pml,options)
    WcaTool.run(pml, options)
    AisExportTool.run(pml,options)
    ApxExportTool.run(pml,options)
    AitAnalyzeTool.run(pml, options)
    AitImportTool.run(pml,options)
    pml.data['timing'].each do |t|
      puts YAML::dump(t)
    end
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
