#
# PLATIN tool set
#
# aiT bridge
#
require 'platin'

module PML
  class OptionParser
    def apx_file(mandatory=true)
      self.on("-a", "--apx FILE", "APX file for a3") { |f| options.apx_file = f }
      self.add_check { |options| die_usage "Option --apx is mandatory" unless options.apx_file } if mandatory
    end
    def ais_file(mandatory=true)
      self.on("--ais FILE", "Path to AIS file") { |f| options.ais_file = f }
      self.add_check { |options| die_usage "Option --ais is mandatory" unless options.ais_file } if mandatory
    end
    def ait_result_file(mandatory=true)
      self.on("-x", "--results FILE", "Path to XML file containing aiT results") { |f| options.ait_result_file = f }
      self.add_check { |options| die_usage "Option --results is mandatory" unless options.ait_result_file } if mandatory
    end
  end

  class AISExporter
    attr_reader :stats_generated_facts,  :stats_skipped_flowfacts
    attr_reader :outfile
    def initialize(pml,ais_file,options)
      @pml = pml
      @outfile = ais_file
      @options = options
      @stats_generated_facts, @stats_skipped_flowfacts = 0, 0
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
      @stats_generated_facts += 1
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
        @stats_skipped_flowfacts += 1
      end
    end

  end

  class APXExporter
    attr_reader :outfile
    def initialize(outfile)
      @outfile = outfile
    end

    def export_project(binary, aisfile, results, report, analysis_entry)
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
      @outfile.puts "   <analysis_start>#{analysis_entry}</analysis_start>"
      @outfile.puts '  </analysis>'
      @outfile.puts ' </analyses>'
      @outfile.puts '</project>'
    end
  end

# end module PML
end
