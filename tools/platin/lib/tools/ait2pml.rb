#!/usr/bin/env ruby
#
# platin tool set
#
# Interprets the results of the aiT analysis in PML
#

require 'platin'
require 'tools/extract-symbols'
require 'ext/ait'
include PML

class AitImportTool
  def AitImportTool.add_config_options(opts)
    opts.on("--[no-]import-addresses", "import memory address range identified during value analysis (=true)") { |b|
      opts.options.ait_import_addresses = b
    }
    opts.add_check { |options| options.ait_import_addresses = true if options.ait_import_addresses.nil? }
  end
  def AitImportTool.add_options(opts)
    ExtractSymbolsTool.add_config_options(opts)
    AitImportTool.add_config_options(opts)
    opts.analysis_entry
    opts.binary_file
    opts.ait_report_prefix
    opts.timing_output("aiT")
    opts.import_block_timing
  end
  def AitImportTool.run(pml,options)
    needs_options(options, :analysis_entry, :ait_report_prefix)
    entry  = pml.machine_functions.by_label(options.analysis_entry, true)
    if ! entry
      die("Analysis entry (ELF label #{options.analysis_entry}) not found")
    end
    unless entry.blocks.first.address
      die("No addresses in PML file, and no binary file given") unless options.binary_file
      warn("No addresses in PML file, trying to extract from ELF file")
      ExtractSymbolsTool.run(pml,options)
    end
    AitImport.new(pml,options).run
    pml
  end
end

# Tool to run a3patmos
# XXX: Internal tool; move into different directory; these tools are not visible on the command line)
class AitAnalyzeTool
  def AitAnalyzeTool.add_config_options(opts)
    opts.on("--a3-command COMMAND", "path to a3patmos executable (=a3patmos)") { |cmd| opts.options.a3 = cmd }
    opts.on("--[no-]ait-persistence-analysis", "enable aiT cache persistence analysis (=true)") { |b|
      opts.options.ait_persistence_analysis = b
    }
    opts.add_check { |options|
      options.a3 = "a3patmos"                 if options.a3.nil?
      options.ait_persistence_analysis = true if options.ait_persistence_analysis.nil?
    }
  end
  def AitAnalyzeTool.add_options(opts, mandatory = true)
    AitAnalyzeTool.add_config_options(opts)
    opts.apx_file(mandatory)
  end
  def AitAnalyzeTool.run(pml, options)
    needs_options(options, :a3, :apx_file)

    unless safe_system("#{options.a3} -b #{options.apx_file}")
      die "aiT (command: '#{options.a3}') failed batch processing #{options.apx_file} (exit status #{$?})"
    end
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
