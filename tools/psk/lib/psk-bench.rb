#!/usr/bin/env ruby
#
require 'utils.rb'
include PML

require 'psk-analyze-trace'
require 'psk-extract-symbols'
require 'psk-pml2ais'
require 'psk-ait2pml'

class BenchTool
  TOOLS = [ExtractSymbolsTool,AnalyzeTraceTool, AisExportTool, 
           ApxExportTool,AitAnalyzeTool,AitImportTool]

  def BenchTool.run(pml,options)
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
  def BenchTool.add_options(opts,options)
    TOOLS.each { |toolclass| toolclass.add_options(opts,options) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT"
EOF
  options, args = PML::optparse(1, "program.elf.pml", SYNOPSIS, :type => :none) do |opts,options|
    BenchTool.add_options(opts,options)
  end
  BenchTool.run(PMLDoc.from_file(args[0]), options)
end
