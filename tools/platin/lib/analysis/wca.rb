#
# platin tool set
#
# "Inhouse" Worst-Case Execution Time Analysis using IPET
#
require 'core/utils'
require 'core/pml'
require 'analysis/ipet'
require 'analysis/cache_region_analysis'
require 'analysis/vcfg'
require 'ext/lpsolve'
require 'ext/gurobi'

module PML

class WCA

  def initialize(pml, options)
    @pml, @options = pml, options
  end

  def analyze(entry_label)

    # Builder and Analysis Entry
    ilp = GurobiILP.new(@options) if @options.use_gurobi
    ilp = LpSolveILP.new(@options) unless ilp

    machine_entry = @pml.machine_functions.by_label(entry_label)
    bitcode_entry = @pml.bitcode_functions.by_name(entry_label)
    entry = { 'machinecode' => machine_entry, 'bitcode' => bitcode_entry }


    # PLAYING: VCFGs
    #bcffs,mcffs = ['bitcode','machinecode'].map { |level|
    #  @pml.flowfacts.filter(@pml,@options.flow_fact_selection,@options.flow_fact_srcs,level)
    #}
    #ctxm = ContextManager.new(@options.callstring_length,1,1,2)
    #mc_model = ControlFlowModel.new(@pml.machine_functions, machine_entry, mcffs, ctxm, @pml.arch)
    #mc_model.build_ipet(ilp) do |edge|
      # pseudo cost (1 cycle per instruction)
    #  if (edge.kind_of?(Block))
    #    edge.instructions.length
    #  else
    #    edge.source.instructions.length
    #  end
    #end

    #cfbc = ControlFlowModel.new(@pml.bitcode_functions, bitcode_entry, bcffs,
    #                            ContextManager.new(@options.callstring_length), GenericArchitecture.new)

    # BEGIN: remove me soon
    # builder
    builder = IPETBuilder.new(@pml, @options, ilp)

    # flow facts
    flowfacts = @pml.flowfacts.filter(@pml,
                                     @options.flow_fact_selection,
                                     @options.flow_fact_srcs,
                                     ["machinecode"],
                                     true)
    ff_levels = ["machinecode"]

    # Build IPET using costs from @pml.arch
    builder.build(entry, flowfacts) do |edge|
      # get list of executed instructions
      branch_index = nil
      ilist =
        if (edge.kind_of?(Block))
          edge.instructions
        else
          src = edge.source
          src.instructions.each_with_index { |ins,ix|
            if ins.returns? && edge.target == :exit
              branch_index = ix # last instruction that returns
            elsif ! ins.branch_targets.empty? && ins.branch_targets.include?(edge.target)
              branch_index = ix # last instruction that branches to the target
            end
          }
          if ! branch_index
            src.instructions
          else
            slots = src.instructions[branch_index].delay_slots
            slot_end = branch_index
            instr = src.instructions[slot_end+1]
            while slots > 0 || (instr && instr.bundled?)
              if ! (instr && instr.bundled?)
                slots = slots - 1
              end
              slot_end = slot_end + 1
              instr = src.instructions[slot_end+1]
            end
            src.instructions[0..slot_end]
          end
        end
      path_wcet = @pml.arch.path_wcet(ilist) 
      edge_wcet = @pml.arch.edge_wcet(ilist,branch_index,edge)
      debug(@options,:costs) { "WCET edge costs for #{edge}: #{path_wcet} block, #{edge_wcet} edge" }
      path_wcet + edge_wcet
    end

    # run cache analyses
    ca = CacheAnalysis.new(builder.refinement['machinecode'], @pml, @options)
    ca.analyze(entry['machinecode'], builder)

    # END: remove me soon

    statistics("WCA",
               "flowfacts" => flowfacts.length,
               "ipet variables" => builder.ilp.num_variables,
               "ipet constraints" => builder.ilp.constraints.length) if @options.stats

    # Solve ILP
    begin
      cycles, freqs = builder.ilp.solve_max
    rescue Exception => ex
      warn("WCA: ILP failed: #{ex}") unless @options.disable_ipet_diagnosis
      cycles,freqs = -1, {}
    end

    # report result
    profile = Profile.new([])
    report = TimingEntry.new(machine_entry, cycles, profile,
                             'level' => 'machinecode', 'origin' => @options.timing_output || 'platin')

    # collect edge timings
    edgefreqs, edgecosts, totalcosts = {}, Hash.new(0), Hash.new(0)
    freqs.each { |v,freq|
      edgecost = builder.ilp.get_cost(v)
      freq = freq.to_i
      if edgecost > 0 || (v.kind_of?(IPETEdge) && v.cfg_edge?)

        next if v.kind_of?(Instruction)         # Stack-Cache Cost
        next if v.kind_of?(IPETEdgeSCA)         # Stack-Cache Cost (graph-based)
        ref = nil
        if v.kind_of?(IPETEdge)
          die("ILP cost: source is not a block") unless v.source.kind_of?(Block)
          die("ILP cost: target is not a block") unless v.target == :exit || v.target.kind_of?(Block)
          ref = ContextRef.new(v.cfg_edge, Context.empty)
          edgefreqs[ref] = freq
        elsif v.kind_of?(MemoryEdge)
          ref = ContextRef.new(v.edgeref, Context.empty)
        end
        edgecosts[ref] += edgecost
        totalcosts[ref] += edgecost*freq
      end
    }
    edgecosts.each { |ref, edgecost|
      unless edgefreqs.include?(ref)
        warn("edge cost (#{ref} -> #{edgecost}), but no corresponding IPETEdge variable")
        next
      end
      edgefreq = edgefreqs[ref]
      profile.add(ProfileEntry.new(ref, edgecost, edgefreqs[ref], totalcosts[ref]))
    }
    ca.summarize(@options, freqs, Hash[freqs.map{ |v,freq| [v,freq * builder.ilp.get_cost(v)] }], report)
    if @options.verbose
      puts "Cycles: #{cycles}"
      puts "Edge Profile:"
      freqs.map { |v,freq|
        [v,freq * builder.ilp.get_cost(v)]
      }.sort_by { |v,freq|
        [v.function || machine_entry, -freq]
      }.each { |v,cost|
        puts "  #{v}: #{freqs[v]} (#{cost} cyc)"
      }
    end
    report
  end
end

end # module PML
