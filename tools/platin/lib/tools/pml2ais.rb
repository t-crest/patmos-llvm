#
# PSK toolset
#
# AIS exporter
#
require 'platin'
require 'ext/ait'
include PML

class AisExportTool
  def AisExportTool.add_config_options(opts)
    opts.on("-g", "--header", "Generate AIS header") { opts.options.ais_header = true }
  end
  def AisExportTool.add_options(opts)
    AisExportTool.add_config_options(opts)
    opts.ais_file(true)
    opts.flow_fact_selection
  end
  def AisExportTool.run(pml, options)
    needs_options(options, :ais_file, :flow_fact_selection, :flow_fact_srcs)

    File.open(options.ais_file, "w") { |outfile|
      ais = AISExporter.new(pml, outfile, options)
      ais.gen_header if options.ais_header

      pml.machine_functions.each { |func| ais.export_jumptables(func) }
      flowfacts = pml.flowfacts.filter(pml, options.flow_fact_selection, options.flow_fact_srcs, ["machinecode"])
      flowfacts.each { |ff| ais.export_flowfact(ff) }

      statistics("AIS flow facts" => ais.stats_generated_facts,
                 "Unsupported flow facts" => ais.stats_skipped_flowfacts) if options.stats
    }
  end
end

class ApxExportTool
  def ApxExportTool.add_config_options(opts)
  end
  def ApxExportTool.add_options(opts, mandatory=true)
    opts.analysis_entry

    opts.apx_file(mandatory)
    opts.binary_file(mandatory)
    opts.ait_result_file(mandatory)

    opts.on("-r", "--report FILE", "Filename of the report log file") { |f| opts.options.ait_report_file = f }
    opts.add_check { |options|
      die_usage "No apx file specified." if mandatory && ! options.apx_file
      if options.apx_file
        die_usage "Option --binary  is mandatory when generating apx file" unless options.binary_file
        die_usage "Option --results is mandatory when generating apx file" unless options.ait_result_file
      end
    }
  end

  def ApxExportTool.run(pml, options)
    needs_options(options, :binary_file, :ais_file, :ait_result_file, :ait_report_file, :analysis_entry)

    File.open(options.apx_file, "w") do |fh|
      apx_exporter = APXExporter.new(fh)
      apx_exporter.export_project(options.binary_file,
                                  options.ais_file,
                                  options.ait_result_file,
                                  options.ait_report_file,
                                  options.analysis_entry)
    end
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
  pml = PMLDoc.from_files([options.input])
  AisExportTool.run(pml, options)

  # TODO make this available as separate platin-tool to to generate only the APX file!?
  if options.apx_file
    ApxExportTool.run(pml, options)
  end
end
