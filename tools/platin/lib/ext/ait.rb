#
# The *platin* toolkit
#
# Bridge to absint's "aiT" WCET analyzer
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
    def ait_report_prefix(mandatory=true)
      self.on("--ait-report-prefix PREFIX", "Path prefix for aiT's report and XML results") {
        |f| options.ait_report_prefix = f
      }
      self.add_check { |options| die_usage "Option --ait-report-prefix is mandatory" unless options.ait_report_prefix } if mandatory
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

    def gen_fact(ais_instr, descr, derived_from=nil)
      @stats_generated_facts += 1
      @outfile.puts(ais_instr+" # "+descr)
      debug(@options,:ait) {
        s = " derived from #{derived_from}" if derived_from
        "Wrote AIS instruction: #{ais_instr}#{s}"
      }
      true
    end

    # Export jumptables for a function
    def export_jumptables(func)
      func.blocks.each do |mbb|
        branches = 0
        mbb.instructions.each do |ins|
          branches += 1 if ins['branch-type'] && ins['branch-type'] != "none"
          if ins['branch-type'] == 'indirect'
            label = ins.block.label
            instr = if ins.address
                      "#{dquote(label)} + #{ins.address - ins.block.address} bytes"
                    else
                      "#{dquote(label)} + #{branches} branches"
                    end
            successors = ins['branch-targets'] ? ins['branch-targets'] : mbb['successors']
            targets = successors.uniq.map { |succ_name|
              dquote(Block.get_label(ins.function.name,succ_name))
            }.join(", ")
            gen_fact("instruction #{instr} branches to #{targets};","jumptable (source: llvm)",ins)
          end
        end
      end
    end

    # export indirect calls
    def export_calltargets(ff, scope, callsite, targets)
      assert("Bad calltarget flowfact: #{ff.inspect}") { scope && scope.context.empty? }

      # no support for context-sensitive call targets
      unless callsite.context.empty?
        warn("aiT: no support for callcontext-sensitive callsites")
        return false
      end

      block = callsite.block
      location = "#{dquote(block.label)} + #{callsite.instruction.address - block.address} bytes"
      called = targets.map { |f| dquote(f.function.label) }.join(", ")
      gen_fact("instruction #{location} calls #{called} ;",
               "global indirect call targets (source: #{ff.origin})",ff)
    end

    # export loop bounds
    def export_loopbound(ff, scope, bound)
      # As we export loop header bounds, we should say the loop header is 'at the end'
      # of the loop (confirmed by absint (Gernot))

      # we do not support symbolic loop bounds yet
      if ff.symbolic_bound?
        warn("aiT: no support for symbolic loop bounds")
        return false
      end
      # context-sensitive facts not yet supported
      unless scope.context.empty?
        warn("aiT: no support for callcontext-sensitive loop bounds")
        return false
      end
      loopname = dquote(scope.reference.loopblock.label)
      gen_fact("loop #{loopname} max #{bound} end ; ",
               "global loop header bound (source: #{ff.origin})",ff)
    end

    # export global infeasibles
    def export_infeasible(ff, scope, pp)
      insname = dquote(pp.block.label)

      # context-sensitive facts not yet supported
      unless scope.context.empty? && pp.context.empty?
        warn("aiT: no support for context-sensitive scopes / program points: #{ff}")
        return false
      end

      # no support for empty basic blocks (typically at -O0)
      if pp.reference.block.instructions.empty?
        warn("aiT: no support for program points referencing empty blocks: #{ff}")
        return false
      end
      gen_fact("instruction #{insname} is never executed ;",
               "globally infeasible block (source: #{ff.origin})",ff)
    end

    def export_linear_constraint(ff)
      terms_lhs, terms_rhs = [],[]
      terms = ff.lhs.dup
      scope = ff.scope

      unless scope.context.empty?
        warn("aiT: no support for context-sensitive scopes: #{ff}")
        return false
      end

      # no support for context-sensitive linear constraints
      unless  terms.all? { |t| t.context.empty? }
        warn("aiT: no support for context-sensitive scopes / program points: #{ff}")
        return false
      end

      assert("export_linear_constraint: not in function scope") { scope.reference.kind_of?(FunctionRef) }

      # no support for edges in aiT
      unless terms.all? { |t| t.ppref.kind_of?(BlockRef) }
        warn("Constraint not supported by aiT (not a block ref): #{ff}")
        return false
      end
      # no support for empty basic blocks (typically at -O0)
      if terms.any? { |t| t.ppref.block.instructions.empty? }
        warn("Constraint not supported by aiT (empty basic block): #{ff})")
        return false
      end

      # Positivty constraints => do nothing
      if ff.rhs >= 0 && terms.all? { |t| t.factor < 0 }
        return true
      end

      scope = scope.function.blocks.first.ref
      terms.push(Term.new(scope,-ff.rhs)) if ff.rhs != 0
      terms.each { |t|
        set = (t.factor < 0) ? terms_rhs : terms_lhs
        set.push("#{t.factor.abs} (#{dquote(t.ppref.block.label)})")
      }
      cmp_op = "<="
      constr = [terms_lhs, terms_rhs].map { |set|
        set.empty? ? "0" : set.join(" + ")
      }.join(cmp_op)
      gen_fact("flow #{constr};",
               "linear constraint on block frequencies (source: #{ff.origin})",ff)
    end

    # export linear-constraint flow facts
    def export_flowfact(ff)
      supported =
        if(ff.symbolic_bound?)
          false
        elsif scope_bound = ff.get_loop_bound
          export_loopbound(ff,*scope_bound)
        elsif scope_cs_targets = ff.get_calltargets
          export_calltargets(ff,*scope_cs_targets)
        elsif scope_pp = ff.get_block_infeasible
          export_infeasible(ff,*scope_pp)
        elsif(ff.blocks_constraint? || ff.scope.reference.kind_of?(FunctionRef))
          export_linear_constraint(ff)
        else
          info("aiT: unsupported flow fact type: #{ff}") unless supported
          false
        end
      @stats_skipped_flowfacts += 1 unless supported
    end

  end

  class APXExporter
    attr_reader :outfile
    def initialize(outfile)
      @outfile = outfile
    end

    def export_project(binary, aisfile, report_prefix, analysis_entry)
      # There is probably a better way to do this .. e.g., use a template file.
      report  = report_prefix + ".txt"
      results = report_prefix + ".xml"
      report_analysis= report_prefix + ".#{analysis_entry}.xml"
      xmlns='xmlns="http://www.absint.com/apx"'
      # XXX TODO: use rexml to build
      @outfile.puts <<EOF
<!DOCTYPE APX>
<project #{xmlns} target="patmos" version="13.04i">
<options #{xmlns}>
 <analyses_options #{xmlns}>
   <extract_annotations_from_source_files #{xmlns}>true</extract_annotations_from_source_files>
   <xml_call_graph>true</xml_call_graph>
   <xml_show_per_context_info>true</xml_show_per_context_info>
   <xml_wcet_path>true</xml_wcet_path>
 </analyses_options>
 <general_options #{xmlns}>
  <include_path #{xmlns}>.</include_path>
  </general_options>
</options>
<files #{xmlns}>
 <executables #{xmlns}>#{File.expand_path binary}</executables>
  <ais #{xmlns}>#{File.expand_path aisfile}</ais>
  <xml_results #{xmlns}>#{File.expand_path results}</xml_results>
  <report #{xmlns}>#{File.expand_path report}</report>
</files>
<analyses #{xmlns}>
 <analysis #{xmlns} enabled="true" type="wcet_analysis" id="aiT">
  <analysis_start #{xmlns}>#{analysis_entry}</analysis_start>
  <xml_report>#{File.expand_path report_analysis}</xml_report>
 </analysis>
</analyses>
</project>
EOF
    end
  end

class AitImport
  attr_reader :pml, :options
  def initialize(pml, options)
    @pml, @options = pml, options
    @routines = {}
    @blocks = {}
    @contexts = {}
  end
  def read_result_file(file)
    doc = Document.new(File.read(file))
    cycles = doc.elements["results/result[1]/cycles"].text.to_i
    scope = pml.machine_functions.by_label(options.analysis_entry).ref
    entry = TimingEntry.new(scope,
                            cycles,
                            'level' => 'machinecode',
                            'origin' => options.timing_output)
    pml.timing.add(entry)
  end
  def read_routines(analysis_elem)
    analysis_elem.each_element("decode/routines/routine") do |elem|
      address = Integer(elem.attributes['address'])
      routine = OpenStruct.new
      routine.instruction = pml.machine_functions.instruction_by_address(address)
      die("Could not find instruction at address #{address}") unless routine.instruction
      die("routine address is not a basic block") unless routine.instruction.block.instructions.first == routine.instruction
      if elem.attributes['loop']
        routine.loop = routine.instruction.block
        die("loop routine is not a loop header") unless routine.loop.loopheader?
      else
        routine.function = routine.instruction.function
        die("routine is not entry block") unless routine.function.entry_block == routine.instruction.block
      end
      routine.name = elem.attributes['name']
      @routines[elem.attributes['id']] = routine
      elem.each_element("block") { |be|
        unless be.attributes['address']
          debug(options,:ait) { "No address for block #{be}" }
          next
        end
        ins = pml.machine_functions.instruction_by_address(Integer(be.attributes['address']))
        @blocks[be.attributes['id']] = ins
        # "aiT block starts in the middle of LLVM block: #{ins}" if(ins.index != 0)
      }
    end
    @routine_names = @routines.values.inject({}) { |memo,r| memo[r.name] = r; memo }
  end
  def read_contexts(contexts_element)
    contexts = {}
    contexts_element.each_element("context") { |elem|
      ctx = OpenStruct.new
      ctx.id = elem.attributes['id']
      ctx.routine = @routines[elem.attributes['routine']]
      if elem.text && elem.text != "no-history"
        ctx.context = elem.text.split(/\s*,\s*/).map { |s|
          callsite_addr, target = s.split("->",2)
          site = pml.machine_functions.instruction_by_address(Integer(callsite_addr))
          if target =~ /\A"(.*)"(?:\[(\d+)\/(\d+)(\.\.)?\])?\Z/
            routine = @routine_names[$1]
            if $2
              peel,last = $2.to_i, $3.to_i
              loopoffset = peel - 1
              loopstep = (peel == last && $3) ? 1 : 0
              LoopContextEntry.new(routine.loop, loopstep, loopoffset, site)
            else
              CallContextEntry.new(site)
            end
          else
            die("invalid contex target: #{target}")
          end
        }
      else
        ctx.context = []
      end
      if @contexts[ctx.id] && @contexts[ctx.id] != ctx
        raise Exception.new("Duplicate context with different meaning: #{ctx.id}")
      end
      @contexts[ctx.id] = Context.from_list(ctx.context)
    }
    contexts
  end
  def run
    analysis_entry  = pml.machine_functions.by_label(options.analysis_entry, true)
    read_result_file(options.ait_report_prefix + ".xml")

    ait_report_file = options.ait_report_prefix + ".#{options.analysis_entry}" + ".xml"
    analysis_task_elem = Document.new(File.read(ait_report_file)).get_elements("a3/wcet_analysis_task").first
    read_routines(analysis_task_elem)
    debug(options,:ait) { |&msgs|
      @routines.each do |id, r|
        msgs.call("Routine #{id}: #{r}")
      end
    }
    # read value analysis results
    read_contexts(analysis_task_elem.get_elements("value_analysis/contexts").first)
    value_analysis_stats = { 'predictable reads' => 0, 'predictable writes' => 0, 'unpredictable' => 0 }
    analysis_task_elem.each_element("value_analysis/value_accesses/value_access") { |e|
      ins = pml.machine_functions.instruction_by_address(Integer(e.attributes['address']))
      e.each_element("value_context") { |ce|
        context = @contexts[ce.attributes['context']]
        ce.each_element("value_step") { |se|

          is_read = se.attributes['type'] == "read"

          # value_step#index ? value_step#mode? value_step#width?
          # value_area#mod? value_area#rem?

          unpredictable = false
          se.each_element("value_area") { |area|
            min,max,rem,mod  = %w{min max rem mod}.map { |k| Integer(area.attributes[k]) if area.attributes[k] }
            unpredictable = true if min != max
          }
          if unpredictable
            value_analysis_stats['unpredictable'] += 1
            prefix, first = "Access #{ins} in #{context}", true
            se.each_element("value_area") { |area|
              if first
                debug(options,:ait) { prefix }
                first = false
              end
              min,max,rem,mod  = %w{min max rem mod}.map { |k| Integer(area.attributes[k]) if area.attributes[k] }
              debug(options,:ait) {
                sprintf("- %s 0x%08x..0x%08x (%d bytes), mod=0x%x rem=0x%x\n",
                        se.attributes['type'],min,max,max-min,mod || -1,rem || -1)
              }
            }
          else
            value_analysis_stats[(is_read) ? 'predictable reads':'predictable writes'] += 1
          end
        }
      }
    }
    statistics("AIT",value_analysis_stats) if options.stats

    # read wcet analysis results
    wcet_elem = analysis_task_elem.get_elements("wcet_analysis").first
    read_contexts(wcet_elem.get_elements("contexts").first)

    # @contexts.each { |cid,ctx|
    #   puts "Context: #{cid} for #{ctx.routine}"
    #   puts "  no history" if ctx.context.empty?
    #   ctx.context.each { |item|
    #     puts "  #{item}"
    #   }
    # }

    @function_count, @function_cost = {} , Hash.new(0)
    @block_count, @block_cost = {},{}
    wcet_elem.each_element("wcet_path") { |e|
      rentry = e.get_elements("wcet_entry").first.attributes["routine"]
      entry = @routines[rentry]
      next unless  entry.function == analysis_entry
      e.each_element("wcet_routine") { |re|
        routine = @routines[re.attributes['routine']]
        if routine.function
          @function_count[routine.function] = re.attributes['count'].to_i
          @function_cost[routine.function] += re.attributes['cumulative_cycles'].to_i
        else
#          @function_cost[routine.loop.function] += re.attributes['cycles'].to_i
        end
        re.each_element("wcet_context") { |ctx_elem|
          context = @contexts[ctx_elem.attributes['context']]
          ctx_elem.each_element("wcet_edge") { |edge|
            next unless edge.attributes['cycles']
            cycles, count = %w{cycles count}.map { |k| edge.attributes[k].to_i }
            source,target = %w{source_block target_block}.map { |k| @blocks[edge.attributes[k]] }
            block = source.block
            (@block_cost[block]||=Hash.new(0))[context] += cycles
            (@block_count[block]||=Hash.new(0))[context] += count if source.index == 0
          }
        }
      }
    }
    debug(options,:ait) { |&msgs|
      @function_count.each { |f,c|
        msgs.call "- function #{f}: #{@function_cost[f].to_f / c.to_f} * #{c}"
      }
      @block_count.each { |b,ctxs|
        msgs.call "- block #{b}"
        ctxs.each { |ctx,c|
          msgs.call(sprintf(" -- #{ctx}: %.2f * #{c}\n",@block_cost[b][ctx].to_f / c.to_f))
        }
      }
    }
    statistics("AIT","imported results" => 1) if options.stats
  end
end

# end module PML
end
