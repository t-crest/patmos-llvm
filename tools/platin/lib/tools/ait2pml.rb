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
  def AitAnalyzeTool.add_config_options(opts)
    opts.on("--a3-command COMMAND", "path to a3patmos executable (=a3patmos)") { |cmd| opts.options.a3 = cmd }
    opts.add_check { |options| options.a3 = "a3patmos" unless options.a3 }
  end
  def AitAnalyzeTool.add_options(opts, mandatory = true)
    AitAnalyzeTool.add_config_options(opts)
    opts.apx_file(mandatory)
  end
  def AitAnalyzeTool.run(pml, options)
    needs_options(options, :a3, :apx_file)

    system("#{options.a3} -b #{options.apx_file}")
    unless $? == 0
      die "aiT (command: '#{options.a3}') failed batch processing #{options.apx_file} (exit status #{$?})"
    end
  end
end

class AitImportTool
  def AitImportTool.add_config_options(opts)
  end
  def AitImportTool.add_options(opts)
    opts.on("--analyze", "run a3patmos") { opts.options.run_ait = true }
    AitAnalyzeTool.add_options(opts, false)
    opts.analysis_entry
    opts.calculates_wcet('aiT-unknown')
    opts.ait_result_file
  end
  def AitImportTool.run(pml,options)
    needs_options(options, :ait_result_file, :analysis_entry, :timing_output)

    AitAnalyzeTool.run(pml,options) if options.run_ait
    doc = Document.new(File.read(options.ait_result_file))
    cycles = doc.elements["results/result[1]/cycles"].text.to_i
    scope = pml.machine_functions.by_label(options.analysis_entry).ref
    entry = TimingEntry.new(scope,
                            cycles,
                            'level' => 'machinecode',
                            'origin' => options.timing_output)
    pml.timing.add(entry)
    statistics("AIT","imported results" => 1) if options.stats
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
  AitImportTool.run(PMLDoc.from_files(options.input), options).dump_to_file(options.output)
end
