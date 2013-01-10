#!/usr/bin/env ruby
#
SYNOPSIS=<<EOF if __FILE__ == $0
Generates .ais file for the analysis of a benchmark
EOF

$:.unshift File.dirname(__FILE__)
require 'utils.rb'
include PMLUtils

require 'psk-analyze-trace'
require 'psk-extract-symbols'
require 'psk-pml2ais'

class BenchTool
  def BenchTool.run(elf,pml,options)
    ExtractSymbolsTool.run(elf,pml,options)
    AnalyzeTraceTool.run(elf,pml)
    AisExportTool.run(pml,options.output,options)
  end
  def BenchTool.main
    options, args = PML::optparse(2, "program.elf program.elf.pml", SYNOPSIS, :type => :none) do |opts,options|
      opts.on("-o", "--output FILE.pais", "AIS file to generate") { |f| options.output = f }
    end
    BenchTool.run(args[0], PML.from_file(args[1]), options)
  end
end

BenchTool.main if __FILE__ == $0
