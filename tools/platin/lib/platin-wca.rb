#!/usr/bin/env ruby
#
# PSK tool set
#
# Simple Worst-Case Analysis using IPET
#
# TODO: support control-flow relation graphs
#
require 'utils'
require 'ipet'
include PML

class WcaTool
  # XXX: loop-local should be block-loop
  # XXX: calltargets-global are supported, but used during construction
  SUPPORTED_FLOW_FACT_TYPES=%w{loop-local loop-function loop-global block-local block-function block-global infeasible-global calltargets-global}
  def WcaTool.run(pml,options)

    # Builder and Analysis Entry
    builder = IPETBuilder.new(pml, options)
    entry = pml.machine_functions.by_label(options.analysis_entry)

    # flow facts
    ff_types = options.flow_fact_types
    ff_types = WcaTool::SUPPORTED_FLOW_FACT_TYPES if ff_types == :supported
    ff_levels = if options.use_relation_graph then ["bitcode","machinecode"] else ["machinecode"] end
    flowfacts = pml.flowfacts.filter(ff_types, options.flow_fact_srcs, ff_levels)

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
          branch_index + pml.delay_slots + 1
        else
          src.instructions.length
        end
      end
    end

    # Add flow facts
    flowfacts.each { |ff| builder.add_flowfact(ff) }

    # Solve ILP
    cycles,freqs = builder.ilp.solve_max
    if options.verbose
      puts "Cycles: #{cycles}"
      freqs.map { |v,freq|
        [v,freq * builder.ilp.get_cost(v)]
      }.sort { |a,b| b[1] <=> a[1] }.each { |v,cost|
        puts "#{v}: #{freqs[v]} (#{cost} cyc)"
      }
    end

    # report result
    report = TimingEntry.new(entry.ref, cycles, 'problemsize' => builder.ilp.constraints.length,
                             'level' => 'machinecode', 'origin' => options.timing_name || 'platin')
    pml.timing.add(report)

    # XXX: playing: fourier-motzkin elimination
    if options.use_relation_graph
      ilp = builder.ilp
      ilp.variables.each do |var|
        if ilp.vartype[var] != :dst
          ilp.eliminate(var)
          if options.debug
            puts ilp
            old_cycles = cycles
            cycles,freqs = ilp.solve_max
            raise Exception.new("Error eliminating #{var}") if cycles != old_cycles
          end
        end
      end
      cycles,freqs = ilp.solve_max
      report = TimingEntry.new(entry.ref, cycles, 'problemsize' => ilp.constraints.length,'level' => 'machinecode',
                               'origin' => (options.timing_name || 'platin')+"-fm")
      pml.timing.add(report)
    end
    pml
  end
  def WcaTool.add_options(opts)
    opts.analysis_entry
    opts.flow_fact_selection
    opts.calculates_wcet
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
  WcaTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
