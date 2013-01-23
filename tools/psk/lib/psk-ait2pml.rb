#!/usr/bin/env ruby
#
# PSK tool set
#
# Converts the result of the aiT analysis to PML format
#

require 'utils'
include PML

require 'rexml/document'
include REXML

class AitAnalyzeTool
  def AitAnalyzeTool.add_options(opts,options)
    opts.on("-a", "--apx FILE", "APX file for a3 (required)") { |f| options.apx = f }
    opts.on("--a3-command COMMAND", "path to a3patmos executable (=a3patmos)") { |cmd|
      options.a3 = cmd
    }
  end
  def AitAnalyzeTool.run(options)
    options.a3 ||= "a3patmos"
    die_usage "Option --apx is mandatory for aiT analysis" unless options.apx
    system("#{options.a3} -b #{options.apx}")
    die "aiT failed batch processing #{options.apx}" unless $? == 0
  end
end

class AitImportTool
  def AitImportTool.add_options(opts,options)
    opts.on("-e", "--analysis-entry FUNCTION", "Name of the function to analyse") { |f| options.analysis_entry = f }
    opts.on("-x", "--results FILE", "Filename of the results xml file") { |f| options.ait_results = f }
  end
           
  def AitImportTool.run(pml,options)
    doc = Document.new(File.read(options.ait_results))
    options.analysis_entry ||= "main"
    cycles = doc.elements["results/result[1]/cycles"].text.to_i
    scope = pml.machine_functions.by_label(options.analysis_entry).ref
    entry = TimingEntry.new(scope, cycles, 'level' => 'machinecode', 'origin' => 'aiT')
    pml.add_timing(entry)
    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Add aiT analysis results to the PML database.
EOF
  options, args = PML::optparse(0, "", SYNOPSIS, :type => :io) do |opts,options|
    AitImportTool.add_options(opts,options)
  end
  AitImportTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
