#!/usr/bin/env ruby
#
# PSK tool set
#
# Converts the result of the aiT analysis to PML format
#

require 'platin'
require 'ext/ait'
include PML

require 'rexml/document'
include REXML

class AitAnalyzeTool
  def AitAnalyzeTool.add_options(opts, mandatory = true)
    opts.apx_file(mandatory)
    opts.on("--a3-command COMMAND", "path to a3patmos executable (=a3patmos)") { |cmd| opts.options.a3 = cmd }
    opts.add_check { |options| options.a3 = "a3patmos" unless options.a3 }
  end
  def AitAnalyzeTool.run(pml, options)
    system("#{options.a3} -b #{options.apx_file}")
    die "aiT (command: '#{options.a3}') failed batch processing #{options.apx_file} (exit status #{$?})" unless $? == 0
  end
end

class AitImportTool
  def AitImportTool.add_options(opts)
    opts.on("--analyze", "run a3patmos") { opts.options.run_ait = true }
    AitAnalyzeTool.add_options(opts, false)
    opts.analysis_entry
    opts.calculates_wcet
    opts.ait_result_file
  end
  def AitImportTool.run(pml,options)
    AitAnalyzeTool.run(pml,options) if options.run_ait
    doc = Document.new(File.read(options.ait_result_file))
    cycles = doc.elements["results/result[1]/cycles"].text.to_i
    scope = pml.machine_functions.by_label(options.analysis_entry).ref
    entry = TimingEntry.new(scope, cycles, 'level' => 'machinecode', 'origin' => options.timing_output || 'aiT-unknown')
    pml.timing.add(entry)
    statistics("imported results from aiT result file" => 1) if options.stats
    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Add aiT analysis results to the PML database.
EOF
  options, args = PML::optparse([], "", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    AitImportTool.add_options(opts)
  end
  AitImportTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
