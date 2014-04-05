#
# PML toolset
#
# FFX/F4 exporter
#
require 'platin'
require 'ext/ffx'
require 'ext/otawa'
include PML

class FFXExportTool

  # TODO There should be a common base class to the FFXExportTool and AISExportTool, providing helper-methods for the
  #      disable-export option and the actual export.

  FF_EXPORT_TYPES = %w{jumptables loop-bounds symbolic-loop-bounds flow-constraints infeasible-code call-targets mem-addresses stack-cache}

  def FFXExportTool.add_config_options(opts)
    # TODO should we name the options --ffx-* to be consistent with the tool name and to distinguish from the SWEET ff format,
    #      or should we keep them as --ff to avoid confusion with F4 export ??
    opts.on("--ffx", "Export flow-facts using F4 instead of FFX") { |d| opts.options.export_ffx = true } 
    opts.on("--ff-input FILE", "the F4/FFX file is merged into the final F4/FFX file. Needs to be the same format as the output format") { |file|
      opts.option.ff_input = file
    }
    opts.on("--ff-disable-exports LIST","F4/FFX information that should not be exported (see --help=ff)") { |list|
      opts.options.ff_disable_export = Set.new(list.split(/\s*,\s*/))
    }
    opts.add_check { |options|
      if options.ff_disable_export.nil?
        options.ff_disable_export = Set.new
      else
        unknown = (options.ff_disable_export - Set[*FF_EXPORT_TYPES])
        unless unknown.empty?
          die("F4/FFX export types #{unknown.to_a} not known. Try --help=ff.")
        end
      end
    }
    opts.register_help_topic('ff') { |io|
      io.puts <<-EOF.strip_heredoc
        == F4/FFX Exporter ==

        The option --ff-disable-export controls which information is not exported
        (default is export everything that is supported) and takes a comma-separated list
        including one or more of the following types of information:

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

  def FFXExportTool.add_options(opts, mandatory=true)
    FFXExportTool.add_config_options(opts)
    opts.ff_file(mandatory)
    opts.flow_fact_selection
  end

  def FFXExportTool.run(pml, options)
    needs_options(options, :ff_file, :flow_fact_selection, :flow_fact_srcs)
    options.ff_disable_export = Set.new unless options.ff_disable_export

    File.open(options.ff_file, "w") { |outfile|
      if options.export_ffx
	ffx = FFXExporter.new(pml, options)
      else
	ffx = F4Exporter.new(pml, outfile, options)
      end

      ffx.merge_file(options.ff_input) unless options.ff_input.nil?

      pml.machine_functions.each { |func| ffx.export_jumptables(func) }
      flowfacts = pml.flowfacts.filter(pml, options.flow_fact_selection, options.flow_fact_srcs, ["machinecode"])
      ffx.export_flowfacts(flowfacts)

      unless options.ff_disable_export.include?('mem-addresses')
        pml.valuefacts.select { |vf|
          vf.level == "machinecode" && vf.origin == "llvm.mc" &&
            vf.ppref.context.empty? &&
            ['mem-address-read', 'mem-address-write'].include?(vf.variable)
        }.each { |vf|
          ffx.export_valuefact(vf)
        }
      end

      unless options.ff_disable_export.include?('stack-cache')
        pml.machine_functions.each { |func|
          func.blocks.each { |mbb|
            mbb.instructions.each { |ins|
              ffx.export_stack_cache_annotation(:fill, ins, ins.sc_fill) if ins.sc_fill
              ffx.export_stack_cache_annotation(:spill, ins, ins.sc_spill) if ins.sc_spill
            }
          }
        }
      end

      if options.export_ffx
        ffx.write(outfile)
      end

      statistics("F4/FFX",
                 "exported flow facts" => ffx.stats_generated_facts,
                 "unsupported flow facts" => ffx.stats_skipped_flowfacts) if options.stats
    }
  end
end

class OSXExportTool
  def OSXExportTool.add_config_options(opts)
  end
  def OSXExportTool.add_options(opts, mandatory=true)
    opts.analysis_entry

    opts.otawa_platform_file(mandatory)

    opts.add_check { |options|
      die_usage "No OTAWA platform description file specified." if mandatory && ! options.otawa_platform_file
    }
  end

  def OSXExportTool.run(pml, options)
    needs_options(options, :otawa_platform_file)

    osx = OSXExporter.new(pml, options)

    File.open(options.otawa_platform_file, "w") do |fh|
      osx.export_platform(fh)
    end
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Export flow information from PML as OTAWA F4/FFX file and generate OTAWA OSX platform config files.
EOF
  options, args = PML::optparse([:input], "file.pml", SYNOPSIS) do |opts|
    FFXExportTool.add_options(opts, false)
    OSXExportTool.add_options(opts, false)
  end
  pml = PMLDoc.from_files([options.input])

  if options.ff_file.nil? and options.otawa_platform_file.nil?
    die_usage("Please speficy at least one of the F4/FF$ output file and the OSX platform file.")
  end

  if options.ff_file
    FFXExportTool.run(pml, options)
  end

  if options.otawa_platform_file
    OSXExportTool.run(pml, options)
  end
end
