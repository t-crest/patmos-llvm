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
# it seems as if the pipe communication is the culprit.

require 'platin'
require 'tools/extract-symbols.rb'
include PML

class AnalyzeTraceTool
  def initialize(pml, options, entry)
    @pml, @options, @entry = pml, options, entry
  end
  def analyze_trace
    trace = @pml.arch.simulator_trace(@options)
    tm = MachineTraceMonitor.new(@pml, trace, @options.trace_entry)
    tm.subscribe(VerboseRecorder.new($dbgs)) if @options.debug
    @scheduler = RecorderScheduler.new(@options.recorders,@entry)
    tm.subscribe(@scheduler)
    tm.run

   if(@scheduler.runs == 0)
      die "Analysis entry '#{@options.analysis_entry}' (pc: #{@entry.address}) never executed"
    end

    @scheduler.recorders.each do |recorder|
      puts "Results of recorder #{recorder}"
      recorder.dump
    end
    die("ok")

   # Collect executed and infeasible blocks
    @executed_functions = Set.new
    @executed_blocks    = {}
    @infeasible_functions = Set.new
    @infeasible_blocks    = Set.new
    @global.results.freqs.each do |block,freq|
      @executed_functions.add(block.function)
      bset = (@executed_blocks[block.function] ||= Set.new)
      bset.add(block)
    end
    @executed_blocks.each do |function, covered|
      function.blocks.each do |block|
        unless covered.include?(block)
          @infeasible_blocks.add(block)
          block.callsites.map { |i| i.callees }.flatten.each { |fname|
            next if fname == "__any__"
            fun = @pml.machine_functions.by_label(fname)
            unless @executed_functions.include?(fun)
              @infeasible_functions.add(fun)
              # Why we need them? -> Infeasible Predicated Calls!
              @infeasible_blocks.add(fun.blocks.first)
            end
          }
        end
      end
    end
    statistics("simulator trace length" => trace.stats_num_items) if @options.stats
  end
  def console_output
    # Verbose Output
    if @options.verbose
      $dbgs.puts "* Global Frequencies"
      $dbgs.puts
      @global.results.dump($dbgs)
      $dbgs.puts
      $dbgs.puts "* Local Frequencies"
      @local.results.each { |scope,r|
        r.dump($dbgs, "Function: #{scope}")
      }
      $dbgs.puts
      $dbgs.puts "* Loop Bounds"
      @loops.results.values.each { |r| r.dump($dbgs) }
      $dbgs.puts "* Executed Functions: #{@executed_blocks.keys.join(", ")}"
    end
    # Console Output
    if ! @options.output
      $stdout.puts "=== Summary of '#{@options.analysis_entry}' observed during " + 
                   "execution of '#{@options.trace_entry}' ==="
      loops_by_fun = Hash.new
      @loops.results.each { |loop, r| (loops_by_fun[loop.function]||=[]).push([loop,r]) }
      @local.results.each { |scope,r|
        $stdout.printf("Function %-30s  Cycles: %15s  Executions: %8d\n",scope,r.cycles,r.runs)
        (loops_by_fun[scope] || []).each { |loop,rl|
          $stdout.printf(" Loop %-30s     Cycles: %15s  Executions: %8d Bounds: %8s\n",
                       loop, rl.cycles, rl.runs, rl.freqs.values[0])
        }
      }
    end
  end
  def export_facts
    outpml = @pml
    outpml = outpml.clone_empty if @options.output_diff

    fact_context = { 'level' => 'machinecode', 'origin' => @options.flow_fact_output || 'trace'}
    globalscope = @entry.ref

    outpml.timing.add(TimingEntry.new(globalscope,@global.results.cycles.max,fact_context))

    flow_facts_before = @pml.flowfacts.length

    # Export globally valid call target sets and infeasible blocks (mandatory for WCET analysis)
    @global.results.calltargets.each do |cs,receiverset|
      next unless cs.unresolved_call?
      outpml.flowfacts.add(FlowFact.calltargets(globalscope, cs, receiverset, fact_context, "calltargets-global"))
    end
    @infeasible_blocks.each do |block|
      outpml.flowfacts.add(FlowFact.block_frequency(globalscope, block, 0..0, fact_context, "infeasible-global"))
    end

    # Export global block frequencies
    if @options.generate_scope_entry
      @global.results.freqs.each do |block,freq|
        outpml.flowfacts.add(FlowFact.block_frequency(globalscope, block, freq, fact_context, "block-global"))
      end
    end

    # Export local block frequencies
    @local.results.each do |function,results|
      scope = function.ref
      results.freqs.each do |block,freq|
        outpml.flowfacts.add(FlowFact.block_frequency(scope, block, freq, fact_context, "block-local"))
      end
    end

    # Export Loops
    @loops.results.values.each do |loopbound|
      loop,freq = loopbound.freqs.to_a[0]
      if ! loop
        raise Exception.new("Inconsistent loop structure - no loop bound frequency record for #{loopbound}")
      end
      outpml.flowfacts.add(FlowFact.block_frequency(loop.loopref, loop, freq, fact_context, "loop-local"))
    end

    # Warn about functions / loops not executed
    @infeasible_functions.each do |function|
      warn "Reachable function #{function} never executed by trace"
    end
    @executed_blocks.each do |function,bset|
      function.loops.each do |block|
        unless bset.include?(block)
          warn "Loop #{block} not executed by trace"
          outpml.flowfacts.add(FlowFact.block_frequency(block.loopref, block, 0..0, fact_context, "loop-local"))
        end
      end
    end
    statistics("extracted flow-flact hypotheses" => outpml.flowfacts.length - flow_facts_before) if @options.stats
    outpml
  end
  def AnalyzeTraceTool.add_config_options(opts)
    Architecture.simulator_options(opts)
    opts.trace_entry
  end

  # Examples for recorder specifications:
  #
  #   g:blc    ==> global recorder (program points distinguished by default callstring length)
  #   g:c:1    ==> call-targets for entry function and direct callees (default callstring length)
  #   g:l/2    ==> loop header bounds (callstring length 2)
  #   g:b/1:2  ==> block frequencies for entry function, callees, and callees of callees (callstring length 1)
  #   f/0:b:0  ==> intraprocedural block frequencies for every function
  #   f/2:b:0  ==> intraprocedural block frequencies for every virtually-inlined (threshold=2) function
  #   f:b:0    ==> intraprocedural block frequencies for every virtually-inlined (threshold=default callstring length) function

  # The default recorder is
  #   globally, record block frequencies, loop bounds and call targets (program points distinguished by default callstring length)
  #   for every virtually-inlined function (threshold = default callstring length), record block frequencies
  DEFAULT_RECORDER_SPEC="g:blc,f:b/0:0"

  def AnalyzeTraceTool.add_options(opts)
    ExtractSymbolsTool.add_config_options(opts)
    AnalyzeTraceTool.add_config_options(opts)
    opts.binary_file(true)
    opts.trace_entry
    opts.analysis_entry
    opts.generates_flowfacts
    opts.on("--recorders LIST", "recorder specification (=#{DEFAULT_RECORDER_SPEC}; see --help-recorders)") { |recorder_spec|
      opts.options.recorders = recorder_spec
    }
    opts.on("--callstring-length INTEGER", "default callstring length used in recorders (=0)") { |recorder_cl|
      opts.options.callstring_length = recorder_cl.to_i
    }
    opts.on_tail("--help-recorders", "help on recorder specifications") {
      RecorderSpecification.help($stderr)
      exit 0
    }
    opts.add_check { |options|
      options.recorders = RecorderSpecification.parse(options.recorder_spec || DEFAULT_RECORDER_SPEC, options.callstring_length || 0)
    }
  end

  def AnalyzeTraceTool.run(pml,options)
    needs_options(options, :analysis_entry, :binary_file)
    entry  = pml.machine_functions.by_label(options.analysis_entry, true)

    if ! entry
      die("Analysis entry (ELF label #{options.analysis_entry}) not found")
    end

    unless entry.blocks.first.address
      warn("No addresses in PML file, trying to extract from ELF file")
      pml = ExtractSymbolsTool.run(pml,options)
      pml.dump_to_file(options.input.first) if options.input.size == 1
    end
    tool = AnalyzeTraceTool.new(pml, options, entry)
    tool.analyze_trace
    tool.console_output
    tool.export_facts
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
  pml = AnalyzeTraceTool.run(PMLDoc.from_files(options.input), options)
  pml.dump_to_file(options.output) if options.output
end
