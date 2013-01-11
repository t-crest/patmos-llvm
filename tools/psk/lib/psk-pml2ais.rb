require File.join(File.dirname(__FILE__),"utils.rb")
include PMLUtils

class AISExporter
    attr_reader :outfile
    def initialize(pml,outfile,options)
      @pml = pml
      @outfile = outfile
      @options = options
    end

    # Generate a global AIS header
    def gen_header(data)
	# TODO get compiler type depending on YAML arch type
	@outfile.puts '#compiler'
	@outfile.puts 'compiler "patmos-llvm";'
	@outfile.puts ''

	#@outfile.puts "#clock rate"
	#@outfile.puts "clock exactly 24 MHz;"
	#@outfile.puts ""

	# TODO any additional header stuff to generate (context, entry, ...)?
    end

    # Export jumptables for a function
    def export_jumptables(func)
      func['blocks'].each do |mbb|
        branches = 0
        next unless mbb['instructions']
        mbb['instructions'].each do |ins|
          branches += 1 if ins['branch-type'] && ins['branch-type'] != "none"
          if ins['branch-type'] == 'indirect'
            label = get_mbb_label(func['name'],mbb['name'])
	    instr = "#{dquote(label)} + #{branches} branches"
            successors = ins['branch-targets'] ? ins['branch-targets'] : mbb['successors']
            targets = successors.uniq.map { |succ_name|
              dquote(get_mbb_label(func['name'],succ_name))
            }.join(", ")
            @outfile.puts "instruction #{instr} branches to #{targets};"
          end
        end
      end
    end

    # export indirect calls
    def export_calltargets(ff)
      caller_candidate = ff['lhs'].select { |term|
        term['factor'] == -1 && term['program-point']['instruction']
      }
      die("Bad calltarget flowfact: #{ff.inspect}") unless caller_candidate.length == 1
      caller = caller_candidate.first['program-point']
      fun = @pml.mf(caller['function'])
      # XXX: the patmos export should deliver blocks in index order, but it doesn't ??
      block = fun['blocks'].find { |b| b['name'] == caller['block'] }
      ins = block['instructions'][caller['instruction']]
      raise Exception.new("Inconsistent instruction index") unless ins && ins['index'] == caller['instruction']
      location = "#{dquote(get_mbb_label(fun,block))} + #{ins['address'] - block['address']} bytes"
      targets = ff['lhs'].map { |term|
        next if term == caller_candidate.first
        die("Bad calltarget flowfact: #{ff.inspect}") unless term['factor'] == 1
        called_fun = @pml.mf(term['program-point']['function'])
        dquote(get_mbb_label(called_fun, "0"))
      }.compact
      @outfile.puts "instruction #{location} calls #{targets.join(", ")} ;"
    end

    # export loop bounds
    def export_loopbound(ff)
      fun  = @pml.mf(ff['scope']['function'])
      # XXX: the patmos export should deliver blocks in index order, but it doesn't ??
      loop = fun['blocks'].find { |b| 
        b['name'] == ff['scope']['loop']
      }

      loopname = dquote(get_mbb_label(fun,loop))
      $stderr.puts "Exporting Loop Bound at #{get_mbb_label(fun,loop)} /"+
        "#{sprintf("0x%x",loop['address'])}" if @options[:verbose]
      @outfile.puts "loop #{loopname} max #{ff['rhs']} ;"
    end

    # export linear-constraint flow facts
    def export_flowfact(ff)
      if(ff['classification'] == 'calltargets-global')
        export_calltargets(ff)
      elsif(ff['classification'] == 'loop-local')
        export_loopbound(ff)
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
    options.ffs_types = ['loop-local','calltargets-global'] unless options.ffs_types
    options.ffs_srcs = 'all' unless options.ffs_srcs
    ais = AISExporter.new(pml, outfile, :verbose => options.verbose)
    ais.gen_header(pml.data) if options.header

    pml['machine-functions'].each do |func|
      ais.export_jumptables(func)
    end
    (pml['flowfacts']||[]).each do |fact|
      next unless fact['level'] == 'machinecode'
      next unless options.ffs_srcs=='all' || options.ffs_srcs.include?(fact['origin'])
      next unless options.ffs_types.include?(fact['classification'])
      ais.export_flowfact(fact)
    end
    outfile.close
  end
  def AisExportTool.add_options(opts,options)
    opts.on("-o", "--output FILE.ais", "AIS file to generate") { |f| options.output = f }
    opts.on("",   "--flow-facts TYPE,..", "Flow facts to export (=loop-local,calltargets-global)") { |ty|
      options.ffs_types = ty.split(/\s*,\s*/)
    }
    opts.on("",   "--flow-facts-from ORIGIN,..", "Elegible Flow Fact Sources (=all)") { |srcs|
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
	@outfile.puts "  <executables>#{binary}</executables>"
	@outfile.puts "  <ais>#{aisfile}</ais>" if aisfile
	@outfile.puts "  <xml_results>#{results}</xml_results>" if results
	@outfile.puts "  <report>#{report}</report>" if report
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
  def ApxExportTool.run(aisfile, options)
    apxfile = options.apx == "-" ? $> : File.new(options.apx, "w")
    apx = APXExporter.new(apxfile)

    entry = options.entry ? options.entry : "main"
    apx.export_project(options.binary, options.output, options.results, options.report, entry)
    
    apxfile.close
  end
  def ApxExportTool.add_options(opts,options)
    opts.on("-a", "--apx FILE", "Generate APX file") { |f| options.apx = f }
    opts.on("-b", "--binary FILE", "ELF filename to use for project file") { |f| options.binary = f }
    opts.on("-e", "--entry FUNCTION", "Name of the function to analyse") { |f| options.entry = f }
    opts.on("-r", "--report FILE", "Filename of the report log file") { |f| options.report = f }
    opts.on("-x", "--results FILE", "Filename of the results xml file") { |f| options.results = f }
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
  AisExportTool.run(PML.from_file(args.first), options.output, options)

  # TODO make this available as separate psk-tool too to generate only the APX file!?
  if options.apx
    ApxExportTool.run(options.output, options)
  end
end
