#!/usr/bin/env ruby
#
# PSK tool set
#
# Simple Worst-Case Analysis using IPET
#
require 'platin'
require 'ext/lpsolve'
include PML

class WcaTool

  def WcaTool.add_config_options(opts)
  end

  def WcaTool.add_options(opts)
    WcaTool.add_config_options(opts)
    opts.analysis_entry
    opts.flow_fact_selection
    opts.callstring_length
    opts.calculates_wcet('wca-unknown')
  end

  def WcaTool.run(pml,options)
    needs_options(options, :analysis_entry, :flow_fact_selection, :flow_fact_srcs, :timing_output)

    # Builder and Analysis Entry
    ilp = LpSolveILP.new(options)

    machine_entry = pml.machine_functions.by_label(options.analysis_entry)
    bitcode_entry = pml.bitcode_functions.by_name(options.analysis_entry)
    entry = { :dst => machine_entry, :src => bitcode_entry }


    # PLAYING: VCFGs
    #bcffs,mcffs = ['bitcode','machinecode'].map { |level|
    #  pml.flowfacts.filter(pml,options.flow_fact_selection,options.flow_fact_srcs,level)
    #}
    #ctxm = ContextManager.new(options.callstring_length,1,1,2)
    #mc_model = ControlFlowModel.new(pml.machine_functions, machine_entry, mcffs, ctxm, pml.arch)
    #mc_model.build_ipet(ilp) do |edge|
      # pseudo cost (1 cycle per instruction)
    #  if (edge.kind_of?(Block))
    #    edge.instructions.length
    #  else
    #    edge.source.instructions.length
    #  end
    #end

    #cfbc = ControlFlowModel.new(pml.bitcode_functions, bitcode_entry, bcffs,
    #                            ContextManager.new(options.callstring_length), GenericArchitecture.new)

    # BEGIN: remove me soon
    # builder
    builder = IPETBuilder.new(pml, options, ilp)

    # flow facts
    flowfacts = pml.flowfacts.filter(pml,
                                     options.flow_fact_selection,
                                     options.flow_fact_srcs,
                                     ["machinecode","bitcode"])
    ff_levels = if options.use_relation_graph then ["bitcode","machinecode"] else ["machinecode"] end

    # Refine Control-Flow Model
    builder.refine(entry, flowfacts)

    # Build IPET using Pseudo-Costs
    builder.build(entry) do |edge|
      # pseudo cost (1 cycle per instruction)
      if (edge.kind_of?(Block))
        edge.instructions.length
      else
        src = edge.source
        branch_index = nil
        src.instructions.each_with_index { |ins,ix|
          if btargets = ins['branch-targets'] # XXX ugly
            if btargets.include?(edge.target.name)
              branch_index = ix
            end
          end
        }
        if branch_index
          branch_index + pml.arch.branch_delay_slots + 1
        else
          src.instructions.length
        end
      end
    end
    # Add flow facts
    flowfacts.each { |ff| builder.add_flowfact(ff) }
    # END: remove me soon

    statistics("WCA",
               "flowfacts" => flowfacts.length,
               "ipet variables" => builder.ilp.num_variables,
               "ipet constraints" => builder.ilp.constraints.length) if options.stats

    # Weak-eliminate auxilliary variables
    unless options.debug
      ilp.variables.each do |var|
        # ilp.eliminate_weak(var) if ilp.costs[var] == 0
      end
    end

    # Solve ILP
    cycles,freqs = builder.ilp.solve_max
    statistics("WCA",
               "ilp variables" => builder.ilp.num_variables,
               "ilp constraints" => builder.ilp.constraints.length) if options.stats

    if options.verbose
      puts "Cycles: #{cycles}"
      freqs.map { |v,freq|
        [v,freq * builder.ilp.get_cost(v)]
      }.sort { |a,b| b[1] <=> a[1] }.each { |v,cost|
        puts "#{v}: #{freqs[v]} (#{cost} cyc)"
      }
    end

    # report result
    report = TimingEntry.new(machine_entry.ref, cycles, 'num_constraints' => builder.ilp.constraints.length,
                             'solvertime' => builder.ilp.solvertime,
                             'level' => 'machinecode', 'origin' => options.timing_output || 'platin')
    pml.timing.add(report)

    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Calculate WCET using lp_solve and a simple timing model
EOF
  options, args = PML::optparse(0, "", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    WcaTool.add_options(opts)
  end
  WcaTool.run(PMLDoc.from_files(options.input), options).dump_to_file(options.output)
end
