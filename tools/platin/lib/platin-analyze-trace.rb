#
# PSK Toolchain
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

require 'utils'
require 'trace'
include PML

class AnalyzeTraceTool
  def AnalyzeTraceTool.add_options(opts)
    opts.binary_file(true)
    opts.trace_entry
    opts.analysis_entry
    opts.pasim
  end

  def AnalyzeTraceTool.run(pml,options)
    needs_options(options, :analysis_entry, :binary_file, :pasim)
    trace = SimulatorTrace.new(options.binary_file, options.pasim)
    tm = MachineTraceMonitor.new(pml, trace)
    tm.subscribe(VerboseRecorder.new($dbgs)) if options.debug
    entry  = pml.machine_functions.by_label(options.analysis_entry)
    global = GlobalRecorder.new(entry)
    loops  = LoopRecorder.new(entry)
    tm.subscribe(global)
    tm.subscribe(loops)
    tm.run

    # Collect executed and infeasible blocks
    executed_blocks = {}
    infeasible_blocks = Set.new
    global.results.freqs.each do |block,freq|
      bset = (executed_blocks[block.function] ||= Set.new)
      bset.add(block)
    end
    executed_blocks.each do |function, covered|
      function.blocks.each do |block|
        unless covered.include?(block)
          infeasible_blocks.add(block)
        end
      end
    end

    if options.verbose
      $dbgs.puts "Global Frequencies"
      global.results.dump($dbgs)
      puts "Loop Bounds"
      loops.results.values.each { |r| r.dump($dbgs) }
      $dbgs.puts "Executed Functions: #{executed_blocks.keys.join(", ")}"
    end

    fact_context = { 'level' => 'machinecode', 'origin' => 'trace'}
    globalscope = entry.ref

    pml.add_timing(TimingEntry.new(globalscope,global.results.cycles.max,fact_context))

    # Export global block frequencies, call targets and infeasible blocks
    global.results.freqs.each do |block,freq|
      pml.flowfacts.add(FlowFact.block_frequency(globalscope, block, freq, fact_context, "block-global"))
    end
    global.results.calltargets.each do |cs,receiverset|
      next unless cs['callees'].include?('__any__')
      pml.flowfacts.add(FlowFact.calltargets(globalscope, cs, receiverset, fact_context, "calltargets-global"))
    end
    infeasible_blocks.each do |block|
      pml.flowfacts.add(FlowFact.block_frequency(globalscope, block, 0..0, fact_context, "infeasible-global"))
    end

    # Export Loops
    loops.results.values.each do |loopbound|
      loop,freq = loopbound.freqs.to_a[0]
      if ! loop
        raise Exception.new("Inconsistent loop structure - no loop bound frequency record for #{loopbound}")
      end
      pml.flowfacts.add(FlowFact.block_frequency(loop.loopref, loop, freq, fact_context, "loop-local"))
    end
    executed_blocks.each do |function,bset|
      function.loops.each do |block|
        unless bset.include?(block)
          warn "Loop #{block} not executed by trace"
          pml.flowfacts.add(FlowFact.block_frequency(block.loopref, block, 0..0, fact_context, "loop-local"))
        end
      end
    end

    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF
Run simulator (patmos: pasim --debug-fmt trace), record execution frequencies
of instructions and generate flow facts. Also records indirect call targets.
EOF

  options, args = PML::optparse( [:binary_file], "program.elf", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    AnalyzeTraceTool.add_options(opts)
  end
  AnalyzeTraceTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
