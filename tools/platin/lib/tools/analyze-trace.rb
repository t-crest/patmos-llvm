#
# PLATIN Toolchain
#
# Generate trace using pasim and extract flow facts (underapproximations)
#
# A word on performance; Currently we have (buildbot, adpcm -O0):
#  pasim >/dev/null .. 18s
#  read from pasim  .. 1m39
#  read and parse   .. 1m52
#  platin-analyze-trace .. 2m
# So there is probably not a lot to optimize in the ruby logic.
#
# Nevertheless, for this tool it might be worth to think of a faster solution;
# it seems as if the pipe communication the cuprit.

require 'platin'
include PML

class AnalyzeTraceTool
  def AnalyzeTraceTool.add_config_options(opts)
    Architecture.simulator_options(opts)
    opts.trace_entry
  end
  def AnalyzeTraceTool.add_options(opts)
    AnalyzeTraceTool.add_config_options(opts)
    opts.binary_file(true)
    opts.trace_entry
    opts.analysis_entry
    opts.generates_flowfacts
  end

  def AnalyzeTraceTool.run(pml,options)
    needs_options(options, :analysis_entry, :binary_file, :pasim)
    trace = pml.arch.simulator_trace(options)
    tm = MachineTraceMonitor.new(pml, trace, options.trace_entry)
    tm.subscribe(VerboseRecorder.new($dbgs)) if options.debug
    entry  = pml.machine_functions.by_label(options.analysis_entry)
    global = GlobalRecorder.new(entry)
    local  = LocalRecorder.new(entry)
    loops  = LoopRecorder.new(entry)
    tm.subscribe(global)
    tm.subscribe(local)
    tm.subscribe(loops)
    tm.run

    # Collect executed and infeasible blocks
    executed_functions = Set.new
    executed_blocks    = {}
    infeasible_functions = Set.new
    infeasible_blocks    = Set.new
    global.results.freqs.each do |block,freq|
      executed_functions.add(block.function)
      bset = (executed_blocks[block.function] ||= Set.new)
      bset.add(block)
    end
    executed_blocks.each do |function, covered|
      function.blocks.each do |block|
        unless covered.include?(block)
          infeasible_blocks.add(block)
          block.callsites.map { |i| i.callees }.flatten.each { |fname|
            next if fname == "__any__"
            fun = pml.machine_functions.by_label(fname)
            unless executed_functions.include?(fun)
              infeasible_functions.add(fun)
              # Why we need them? -> Infeasible Predicated Calls!
              infeasible_blocks.add(fun.blocks.first)
            end
          }
        end
      end
    end


    if options.verbose
      $dbgs.puts "* Global Frequencies"
      $dbgs.puts
      global.results.dump($dbgs)
      $dbgs.puts
      $dbgs.puts "* Local Frequencies"
      local.results.each { |scope,r|
        r.dump($dbgs, "Function: #{scope}")
      }
      $dbgs.puts
      $dbgs.puts "* Loop Bounds"
      loops.results.values.each { |r| r.dump($dbgs) }
      $dbgs.puts "* Executed Functions: #{executed_blocks.keys.join(", ")}"
    end

    fact_context = { 'level' => 'machinecode', 'origin' => options.flow_fact_output || 'trace'}
    globalscope = entry.ref

    pml.timing.add(TimingEntry.new(globalscope,global.results.cycles.max,fact_context))

    flow_facts_before = pml.flowfacts.length

    # Export global block frequencies, call targets and infeasible blocks
    global.results.freqs.each do |block,freq|
      pml.flowfacts.add(FlowFact.block_frequency(globalscope, block, freq, fact_context, "block-global"))
    end
    global.results.calltargets.each do |cs,receiverset|
      next unless cs.unresolved_call?
      pml.flowfacts.add(FlowFact.calltargets(globalscope, cs, receiverset, fact_context, "calltargets-global"))
    end
    infeasible_blocks.each do |block|
      pml.flowfacts.add(FlowFact.block_frequency(globalscope, block, 0..0, fact_context, "infeasible-global"))
    end

    # Export local block frequencies
    local.results.each do |function,results|
      scope = function.ref
      results.freqs.each do |block,freq|
        pml.flowfacts.add(FlowFact.block_frequency(scope, block, freq, fact_context, "block-local"))
      end
    end

    # Export Loops
    loops.results.values.each do |loopbound|
      loop,freq = loopbound.freqs.to_a[0]
      if ! loop
        raise Exception.new("Inconsistent loop structure - no loop bound frequency record for #{loopbound}")
      end
      pml.flowfacts.add(FlowFact.block_frequency(loop.loopref, loop, freq, fact_context, "loop-local"))
    end

    # Warn about functions / loops not executed
    infeasible_functions.each do |function|
      warn "Reachable function #{function} never executed by trace"
    end
    executed_blocks.each do |function,bset|
      function.loops.each do |block|
        unless bset.include?(block)
          warn "Loop #{block} not executed by trace"
          pml.flowfacts.add(FlowFact.block_frequency(block.loopref, block, 0..0, fact_context, "loop-local"))
        end
      end
    end
    statistics("simulator trace length" => trace.stats_num_items,
               "extracted flow-flact hypotheses" => pml.flowfacts.length - flow_facts_before) if options.stats
    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF
Run simulator (patmos: pasim --debug-fmt trace), record execution frequencies
of instructions and generate flow facts. Also records indirect call targets.
EOF
  # FIXME: binary file is passed as positional argument, and thus should not be shown
  # as option argument in usage
  options, args = PML::optparse( [:binary_file], "program.elf", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    AnalyzeTraceTool.add_options(opts)
  end
  AnalyzeTraceTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
