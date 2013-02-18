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
  SUPPORTED_FLOW_FACT_TYPES=%w{loop-local loop-function loop-global block-local block-function block-global infeasible-global}
  def WcaTool.run(pml,options)
    options.entry = "main" unless options.entry
    builder = IPETBuilder.new(pml, options)
    entry = pml.machine_functions.by_label(options.analysis_entry)
    pml.flowfacts.each do |ff|
      # set indirect call targets
      model = (ff.level == "machinecode") ? :dst : :src
      scope,cs,targets = ff.get_calltargets
      if scope && scope.kind_of?(FunctionRef) && scope.function == entry
        builder.add_calltargets(cs.instruction, targets, model)
      end
      # set infeasible blocks
      scope,bref,frequency = ff.get_block_frequency_bound
      if scope && scope.kind_of?(FunctionRef) && scope.function == entry && frequency == 0
        builder.set_infeasible(bref.block, model)
      end
    end
    # build IPET, including cost
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
    # add flowfacts
    ff_types = options.flow_fact_types
    ff_types = WcaTool::SUPPORTED_FLOW_FACT_TYPES if ff_types == :supported
    pml.flowfacts.each do |ff|
      # skip if no sources where specified, the level is bitcode, and no relation graph should be used
      next if options.flow_fact_srcs == "all" && ff.level == "bitcode" && ! options.use_relation_graph
      # skip unless the source of the flow fact should be used
      next unless options.flow_fact_srcs == "all" || options.flow_fact_srcs.include?(ff.origin)

      # skip unless that kind of flow fact should be used
      # XXX: sweet flowfacts are not classified yet...
      next unless ff_types.include?(ff.classification) || ! ff.classification
      builder.add_flowfact(ff)
    end
    # solve ILP
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
    report = TimingEntry.new(entry.ref, cycles, 'problemsize' => builder.ilp.constraints.length, 'level' => 'machinecode', 'origin' => options.timing_name || 'platin')
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
      report = TimingEntry.new(entry.ref, cycles, 'problemsize' => ilp.constraints.length,'level' => 'machinecode', 'origin' => (options.timing_name || 'platin')+"-fm")
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
