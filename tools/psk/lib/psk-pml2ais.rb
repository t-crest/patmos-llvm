#
# PSK toolset
#
# AIS exporter
#
require 'utils'
require 'ait'
include PML

class AisExportTool
  def AisExportTool.run(pml, options)
    File.open(options.ais_file, "w") { |outfile|
      ais = AISExporter.new(pml, outfile, options)
      ais.gen_header if options.ais_header

      pml.machine_functions.each do |func|
        ais.export_jumptables(func)
      end
      pml.flowfacts.each do |fact|
        next unless fact['level'] == 'machinecode'
        next unless options.ffs_srcs=='all' || options.ffs_srcs.include?(fact['origin'])
        next unless options.ffs_types.include?(fact.classification)
        ais.export_flowfact(fact)
      end
    }
  end

  def AisExportTool.add_options(opts)
    opts.ais_file(true)
    opts.on("-g", "--header", "Generate AIS header") { |f| opts.options.ais_header = f }
    opts.on("--flow-facts TYPE,..", "Flow facts to export (=loop-local,calltargets-global,infeasible-global)") { |ty|
      opts.options.ffs_types = ty.split(/\s*,\s*/)
    }
    opts.on("--flow-facts-from ORIGIN,..", "Elegible Flow Fact Sources (=all)") { |srcs|
      opts.options.ffs_srcs = srcs.split(/\s*,\s*/)
    }
    opts.add_check { |options|
      options.ffs_types = ['loop-local','calltargets-global','infeasible-global'] unless options.ffs_types
      options.ffs_srcs = 'all' unless options.ffs_srcs
    }
  end
end

class ApxExportTool
  def ApxExportTool.run(pml, options)
    File.open(options.apx_file, "w") { |fh|
      apx_exporter = APXExporter.new(fh)
      apx_exporter.export_project(options.binary_file, options.ais_file, options.ait_result_file, options.report_file,
                                  options.analysis_entry)
    }
  end
  def ApxExportTool.add_options(opts, mandatory=true)
    opts.analysis_entry

    opts.apx_file(mandatory)
    opts.binary_file(mandatory)
    opts.ait_result_file(mandatory)

    opts.on("-r", "--report FILE", "Filename of the report log file") { |f| opts.options.report_file = f }
    opts.add_check { |options|
      die_usage "No apx file specified." if mandatory && ! options.apx_file
      if options.apx_file
        die_usage "Option --report  is mandatory when generating apx file" unless options.report_file
        die_usage "Option --binary  is mandatory when generating apx file" unless options.binary_file
        die_usage "Option --results is mandatory when generating apx file" unless options.ait_result_file
      end
    }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Extract flow information from PML file and export as AbsInt AIS file.
EOF
  options, args = PML::optparse([:input], "file.pml", SYNOPSIS) do |opts|
    AisExportTool.add_options(opts)
    ApxExportTool.add_options(opts, false)
  end
  pml = PMLDoc.from_file(args.first)
  AisExportTool.run(pml, options)

  # TODO make this available as separate psk-tool to to generate only the APX file!?
  if options.apx_file
    ApxExportTool.run(pml, options)
  end
end
