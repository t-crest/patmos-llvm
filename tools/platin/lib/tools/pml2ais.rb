#
# PSK toolset
#
# AIS exporter
#
require 'platin'
require 'ext/ait'
require 'core/utils'
include PML

class AisExportTool

  AIS_EXPORT_TYPES = %w{header jumptables loop-bounds symbolic-loop-bounds flow-constraints infeasible-code call-targets mem-addresses stack-cache}

  def AisExportTool.add_config_options(opts)
    opts.on("--ais-header-file FILE", "the contents of this file is copied verbatim to the final AIS file") { |file|
      opts.options.ais_header_file = file
    }
    ERBTemplate.add_config_options(opts)
    opts.on("--ais-disable-exports LIST","AIS information that should not be exported (see --help=ais)") { |list|
      opts.options.ais_disable_export = Set.new(list.split(/\s*,\s*/))
    }
    opts.add_check { |options|
      if options.ais_disable_export.nil?
        options.ais_disable_export = Set.new
      else
        unknown = (options.ais_disable_export - Set[*AIS_EXPORT_TYPES])
        unless unknown.empty?
          die("AIS export types #{unknown.to_a} not known. Try --help=ais.")
        end
      end
    }
    opts.register_help_topic('ais') { |io|
      io.puts <<-EOF.strip_heredoc
        == AIS Exporter ==

        The option --ais-disable-exports controls which information is not exported
        (default is export everything) and takes a comma-separated list
        including one or more of the following types of information:

        header               ... specification of the compiler
        jumptables           ... targets of indirect branches
        loop-bounds          ... all loop bound specifications
        symbolic-loop-bounds ... loop bounds that depend on the value of an argument/register
        flow-constraints     ... linear flow constraints
        infeasible-code      ... program points that are never executed
        call-targets         ... targets of (indirect) function calls
        mem-addresses        ... value ranges of accesses memory addresses
        stack-cache          ... information about stack cache behavior
        EOF
    }
  end

  # return the list of exports (filter the ones given in the 'except' argument)
  def AisExportTool.get_exports_list(except = [])
    unknown = (Set[*except] - Set[*AIS_EXPORT_TYPES])
    assert("unknown export(s): #{unknown.map {|e| e}.join(',')}") { unknown.empty? }
    return Set[*AIS_EXPORT_TYPES] - Set[*except]
  end

  def AisExportTool.add_options(opts)
    AisExportTool.add_config_options(opts)
    opts.ais_file(true)
    opts.flow_fact_selection
    opts.ait_sca_type
  end

  def AisExportTool.run(pml, options)
    needs_options(options, :ais_file, :flow_fact_selection, :flow_fact_srcs)
    options.ais_disable_export = Set.new unless options.ais_disable_export

    File.open(options.ais_file, "w") { |outfile|
      ais = AISExporter.new(pml, outfile, options)
      ais.export_header unless options.ais_disable_export.include?('header')

      pml.machine_functions.each { |func| ais.export_jumptables(func) }
      flowfacts = pml.flowfacts.filter(pml, options.flow_fact_selection, options.flow_fact_srcs, ["machinecode"])
      ais.export_flowfacts(flowfacts)

      unless options.ais_disable_export.include?('mem-addresses')
        pml.valuefacts.select { |vf|
          vf.level == "machinecode" && vf.origin == "llvm.mc" &&
            vf.ppref.context.empty? &&
            ['mem-address-read', 'mem-address-write'].include?(vf.variable)
        }.each { |vf|
          ais.export_valuefact(vf)
        }
      end

      unless options.ais_disable_export.include?('stack-cache')
        pml.machine_functions.each { |func|
          func.blocks.each { |mbb|
            mbb.instructions.each { |ins|
              ais.add_stack_cache_inst(:reserve, ins, ins.sc_arg) if ins.opcode == "SRESi"
              ais.add_stack_cache_inst(:free, ins, ins.sc_arg) if ins.opcode == "SFREEi"
              ais.add_stack_cache_inst(:ensure, ins, ins.sc_arg) if ins.opcode == "SENSi"
              #ais.export_stack_cache_update(:spill, ins, ins.sc_spill) if ins.sc_spill
            }
          }
        }
        ais.export_stack_cache_annotations()
      end

      ERBTemplate.new(pml, options, options.ff_template_file).render(outfile) if options.ff_template_file
      statistics("AIS",
                 "exported flow facts" => ais.stats_generated_facts,
                 "unsupported flow facts" => ais.stats_skipped_flowfacts) if options.stats
    }
  end
end

class ApxExportTool
  def ApxExportTool.add_config_options(opts)
    opts.ait_icache_mode
    opts.ait_dcache_mode
    opts.ait_sca_type
  end
  def ApxExportTool.add_options(opts, mandatory=true)
    opts.analysis_entry

    opts.apx_file(mandatory)
    opts.binary_file(mandatory)
    opts.ait_report_prefix(mandatory)

    opts.add_check { |options|
      die_usage "No apx file specified." if mandatory && ! options.apx_file
      if options.apx_file
        die_usage "Option --binary  is mandatory when generating apx file" unless options.binary_file
        die_usage "Option --ait-report-prefix is mandatory when generating apx file" unless options.ait_report_prefix
      end
    }
  end

  def ApxExportTool.run(pml, options)
    needs_options(options, :binary_file, :ais_file, :ait_report_prefix, :analysis_entry)

    File.open(options.apx_file, "w") do |fh|
      apx_exporter = APXExporter.new(fh, pml, options)
      apx_exporter.export_project(options.binary_file,
                                  options.ais_file,
                                  options.ait_report_prefix,
                                  options.analysis_entry)
    end
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Extract flow information from PML file and export as AbsInt AIS file.
EOF
  options, args = PML::optparse([], "", SYNOPSIS) do |opts|
    opts.needs_pml
    AisExportTool.add_options(opts)
    ApxExportTool.add_options(opts, false)
  end
  pml = PMLDoc.from_files(options.input)
  AisExportTool.run(pml, options)

  # TODO make this available as separate platin-tool to to generate only the APX file!?
  if options.apx_file
    ApxExportTool.run(pml, options)
  end
end
