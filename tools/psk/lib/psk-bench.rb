#!/usr/bin/env ruby
#
require 'utils.rb'
include PML

require 'psk-analyze-trace'
require 'psk-extract-symbols'
require 'psk-pml2ais'

class BenchTool
  def BenchTool.run(elf,pml,options)
    ExtractSymbolsTool.run(elf,pml,options)
    AnalyzeTraceTool.run(elf,pml)
    AisExportTool.run(pml,options.output,options)
  end
  def BenchTool.add_options(opts,options)
    ExtractSymbolsTool.add_options(opts,options)
    AnalyzeTraceTool.add_options(opts,options)
    AisExportTool.add_options(opts,options)
  end
  def BenchTool.main
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Generates .ais file for the analysis of a benchmark
EOF
  options, args = PML::optparse(2, "program.elf program.elf.pml", SYNOPSIS, :type => :none) do |opts,options|
    BenchTool.add_options(opts,options)
  end
  BenchTool.run(args[0], PMLDoc.from_file(args[1]), options)
end
