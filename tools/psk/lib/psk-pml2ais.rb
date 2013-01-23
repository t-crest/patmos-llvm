#
# PSK toolset
#
# AIS exporter
#
require 'utils'
include PML

class AISExporter
    attr_reader :outfile
    def initialize(pml,outfile,options)
      @pml = pml
      @outfile = outfile
      @options = options
    end

    # Generate a global AIS header
    def gen_header
	# TODO get compiler type depending on YAML arch type
	@outfile.puts '#compiler'
	@outfile.puts 'compiler "patmos-llvm";'
	@outfile.puts ''

	#@outfile.puts "#clock rate"
	#@outfile.puts "clock exactly 24 MHz;"
	#@outfile.puts ""

	# TODO any additional header stuff to generate (context, entry, ...)?
    end

    def gen_fact(ais_instr, descr)
      @outfile.puts(ais_instr+" # "+descr)
      $stderr.puts(ais_instr) if @options.verbose
    end

    # Export jumptables for a function
    def export_jumptables(func)
      func.blocks.each do |mbb|
        branches = 0
        mbb.instructions.each do |ins|
          branches += 1 if ins['branch-type'] && ins['branch-type'] != "none"
          if ins['branch-type'] == 'indirect'
            label = ins.block.label
	    # instr = "#{dquote(label)} + #{branches} branches"
            instr = "#{dquote(label)} + #{ins.address - ins.block.address} bytes"
            successors = ins['branch-targets'] ? ins['branch-targets'] : mbb['successors']
            targets = successors.uniq.map { |succ_name|
              dquote(Block.get_label(ins.function.name,succ_name))
            }.join(", ")
            gen_fact("instruction #{instr} branches to #{targets};","jumptable (source: llvm)")
          end
        end
      end
    end

    # export indirect calls
    def export_calltargets(ff)
      scope, callsite, targets = ff.get_calltargets
      assert("Bad calltarget flowfact: #{ff.inspect}") { scope }
      block = callsite.block
      location = "#{dquote(block.label)} + #{callsite.instruction.address - block.address} bytes"
      called = targets.map { |f| dquote(f.label) }.join(", ")
      gen_fact("instruction #{location} calls #{called} ;",
               "global indirect call targets (source: #{ff['origin']})")
    end

    # export loop bounds
    def export_loopbound(ff)
      # FIXME: As we export loop header bounds, we should say the loop header is 'at the end' 
      # of the loop. But apparently this is not how loop bounds are interpreted in
      # aiT (=> ask absint guys)
      loopname = dquote(ff.scope.loopblock.label)
      gen_fact("loop #{loopname} max #{ff.rhs} ;", # end ;"
               "local loop header bound (source: #{ff['origin']})")
    end

    # export global infeasibles
    def export_infeasible(ff)
      scope, pp, zero = ff.get_block_frequency_bound
      insname = dquote(pp.block.label)

      # XXX: Hack: only export infeasible calls (minimum neccessary to calculate WCET)
      if pp.block.calls?
        gen_fact("instruction #{insname} is never executed ;",
                 "globally infeasible call (source: #{ff['origin']})")
      end
    end

    # export linear-constraint flow facts
    def export_flowfact(ff)
      if(ff.classification == 'calltargets-global')
        export_calltargets(ff)
      elsif(ff.classification == 'loop-local')
        export_loopbound(ff)
      elsif(ff.classification == 'infeasible-global')
        export_infeasible(ff)
      else
        die ("General purpose flow-fact generation not yet implemented")
      end
    end

end

class AisExportTool
  def AisExportTool.run(pml, of, options)
    outfile = if ! of || of == "-"
              then $>
              else File.new(of,"w")
              end
    # export
    options.ffs_types = ['loop-local','calltargets-global','infeasible-global'] unless options.ffs_types
    options.ffs_srcs = 'all' unless options.ffs_srcs
    ais = AISExporter.new(pml, outfile, options)
    ais.gen_header if options.header

    pml.machine_functions.each do |func|
      ais.export_jumptables(func)
    end
    pml.flowfacts.each do |fact|
      next unless fact['level'] == 'machinecode'
      next unless options.ffs_srcs=='all' || options.ffs_srcs.include?(fact['origin'])
      next unless options.ffs_types.include?(fact.classification)
      ais.export_flowfact(fact)
    end
    outfile.close
  end
  def AisExportTool.add_options(opts,options)
    opts.on("--ais FILE", "AIS file to generate") { |f| options.ais = f }
    opts.on("--flow-facts TYPE,..", "Flow facts to export (=loop-local,calltargets-global,infeasible-global)") { |ty|
      options.ffs_types = ty.split(/\s*,\s*/)
    }
    opts.on("--flow-facts-from ORIGIN,..", "Elegible Flow Fact Sources (=all)") { |srcs|
      options.ffs_srcs = srcs.split(/\s*,\s*/)
    }
    opts.on("-g", "--header", "Generate AIS header") { |f| options.header = f }
  end
end

class APXExporter
    attr_reader :outfile
    def initialize(outfile)
	@outfile = outfile
    end

    def export_project(binary, aisfile, results, report, entry)
	# There is probably a better way to do this .. e.g., use a template file.
	@outfile.puts '<!DOCTYPE APX>'
	@outfile.puts '<project xmlns="http://www.absint.com/apx" target="patmos" version="12.10i">'
	@outfile.puts ' <files>'
	@outfile.puts "  <executables>#{File.expand_path binary}</executables>"
	@outfile.puts "  <ais>#{File.expand_path aisfile}</ais>" if aisfile
	@outfile.puts "  <xml_results>#{File.expand_path results}</xml_results>" if results
	@outfile.puts "  <report>#{File.expand_path report}</report>" if report
	@outfile.puts ' </files>'
	@outfile.puts ' <analyses>'
	@outfile.puts '  <analysis enabled="true" type="wcet_analysis" id="aiT">'
	@outfile.puts "   <analysis_start>#{entry}</analysis_start>"
	@outfile.puts '  </analysis>'
	@outfile.puts ' </analyses>'
	@outfile.puts '</project>'
    end
end

class ApxExportTool
  def ApxExportTool.run(pml, options)
    apxfile = options.apx == "-" ? $> : File.new(options.apx, "w")
    entry = options.analysis_entry || "main"

    apx = APXExporter.new(apxfile)
    apx.export_project(options.binary, options.ais, options.ait_results, options.report, entry)    
    apxfile.close
  end
  def ApxExportTool.add_options(opts,options)
    opts.on("-a", "--apx FILE", "Generate APX file") { |f| options.apx = f }
    opts.on("-b", "--binary FILE", "ELF filename to use for project file") { |f| options.binary = f }
    opts.on("-e", "--analysis-entry FUNCTION", "Name of the function to analyse") { |f| options.analysis_entry = f }
    opts.on("-r", "--report FILE", "Filename of the report log file") { |f| options.report = f }
    opts.on("-x", "--results FILE", "Filename of the results xml file") { |f| options.ait_results = f }
  end
  def ApxExportTool.check_options(options)
    if options.apx and !options.binary then
      $stderr.puts "Exporting an APX file requires setting a binary filename"
      exit 1
    end
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Extract flow information from PML file and export as AbsInt AIS file.
EOF
  options, args = PML::optparse(1..1, "file.pml", SYNOPSIS, :type => :none) do |o|
    AisExportTool.add_options(*o)
    ApxExportTool.add_options(*o)
  end
  pml = PMLDoc.from_file(args.first)
  AisExportTool.run(pml, options.ais, options)

  # TODO make this available as separate psk-tool too to generate only the APX file!?
  if options.apx
    ApxExportTool.run(pml, options)
  end
end
