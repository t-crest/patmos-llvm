#!/usr/bin/env ruby
#
require 'utils.rb'
include PML

require 'psk-analyze-trace'
require 'psk-extract-symbols'
require 'psk-pml2ais'
require 'psk-ait2pml'
require 'psk-ff2pml'

class BenchToolSweet
  TOOLS = [SweetAnalyzeTool,SweetImportTool,
           ExtractSymbolsTool,AnalyzeTraceTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool]

  def BenchToolSweet.run(pml,options)
    SweetAnalyzeTool.run(options)
    SweetImportTool.run(options.sweet_ff_file, pml, options)
    ExtractSymbolsTool.run(options.binary,pml,options)
    AnalyzeTraceTool.run(options.binary,pml,options)
    AisExportTool.run(pml,options.ais,options)
    ApxExportTool.run(pml,options)
    AitAnalyzeTool.run(options)
    AitImportTool.run(pml,options)
    pml.data['timing'].each do |t|
      puts YAML::dump(t)
    end
    pml
  end
  def BenchToolSweet.add_options(opts,options)
    TOOLS.each { |toolclass| toolclass.add_options(opts,options) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT"
EOF
  options, args = PML::optparse(1, "program.elf.pml", SYNOPSIS, :type => :none) do |opts,options|
    BenchToolSweet.add_options(opts,options)
  end
  BenchToolSweet.run(PMLDoc.from_file(args[0]), options)
end
