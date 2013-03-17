#
# PLATIN tool set
#
# trace analysis (similar to SWEET single path mode)
#
require 'core/utils'
require 'core/pml'

module PML

require 'arch/patmos'

# Class to monitor traces and generate events
# should implement a 'run' method
class TraceMonitor
  attr_reader :observers
  def initialize
    @observers = []
  end
  def subscribe(obj)
    @observers.push(obj)
  end
  def publish(msg,*args)
    @observers.each do |obs|
      obs.send(msg,*args)
    end
  end
end

#
# Class to monitor machine trace and generate events
#
# Traces need to be of the form [programcounter, cycles]
# Generated events: block, function, ret, loop{enter,exit,cont}, eof
#
class MachineTraceMonitor < TraceMonitor
  def initialize(pml,trace,program_start = "main")
    super()
    @pml = pml
    @delay_slots = pml.delay_slots
    @trace = trace
    @program_entry = @pml.machine_functions.by_label(program_start)
    @start = @program_entry.blocks.first.address
    # whether an instruction is a watch point
    @wp = {}
    # basic block watch points
    @wp_block_start = {}
    # call instruction watch points
    @wp_call_instr = {}
    # instructions which the callee returns to
    @wp_callreturn_instr = {}
    # return instruction watch points
    @wp_return_instr = {}
    # empty (zero-size) blocks before the key'ed block
    @empty_blocks = {}
    build_watchpoints
  end
  # run monitor
  def run
    @executed_instructions = 0
    @callstack = []
    @loopstack = nil
    @current_function = nil

    pending_return, pending_call = nil, nil
    @trace.each do |pc,cycles|

      @started = true if pc == @start
      next unless @started

      @executed_instructions += 1
      next unless @wp[pc] || pending_return

      @cycles = cycles

      # Handle Basic Block
      if b = @wp_block_start[pc]

        # function entry
        if b.address == b.function.address

          # call
          if pending_call
            handle_call(*pending_call) if pending_call
            #puts "Call: #{pending_call.first} -> #{b.function}"
            pending_call = nil
          else
            assert("Empty call history at function entry, but not main function (#{b.function},#{@program_entry})") {
              b.function == @program_entry
            }
          end

          # set current function
          @current_function = b.function
          @loopstack = []
          publish(:function, b.function, @callstack[-1], @cycles)
        end

        # loop exit
        exit_loops_downto(b.loopnest)

        # loop header
        handle_loopheader(b)

        # basic block
        assert("Current function does not match block: #{@current_function} != #{b}") { @current_function == b.function }
        @empty_blocks[b.address].each { |b0| publish(:block, b0, @cycles) } if @empty_blocks[b.address]
        publish(:block, b, @cycles)
      end

      # Handle Call
      if c = @wp_call_instr[pc]
        pending_call = [c, @executed_instructions]
      end

      # Handle Return Block (TODO: in order to handle predicated returns, we need to know where return instructions are)
      if r = @wp_return_instr[pc]
        pending_return = [r,@executed_instructions]
      end
      # Execute return
      if pending_return && pending_return[1] + @delay_slots == @executed_instructions
        #puts "Return from #{pending_return.first} -> #{@callstack[-1]}"
        break unless handle_return(*pending_return)
        pending_return = nil
      end
    end

    publish(:eof)
  end

  private

  def handle_loopheader(b)
    if b.loopheader?
      if b.loopnest == @loopstack.length && @loopstack[-1].name != b.name
        publish(:loopexit, @loopstack.pop, @cycles)
      end
      if b.loopnest == @loopstack.length
        publish(:loopcont, b, @cycles)
      else
        @loopstack.push b
        publish(:loopenter, b, @cycles)
      end
    end
  end

  def handle_call(c, call_pc)
    assert("No call instruction before function entry #{call_pc + 1 + @delay_slots} != #{@executed_instructions}") {
      call_pc + 1 + @delay_slots == @executed_instructions
    }
    @callstack.push(c)
  end

  def handle_return(r, ret_pc)
    exit_loops_downto(0)
    if @callstack.empty?
      publish(:ret, r, nil, @cycles)
    else
      publish(:ret, r, @callstack[-1], @cycles)
    end
    return nil if(r.function == @program_entry)
    assert("Callstack empty at return (inconsistent callstack)") { ! @callstack.empty? }
    c = @callstack.pop
    @loopstack = c.block.loops.reverse
    @current_function = c.function
  end

  def exit_loops_downto(nest)
    while nest < @loopstack.length
      publish(:loopexit, @loopstack.pop, @cycles)
    end
  end

  def build_watchpoints
    # generate watchpoints for all relevant machine functions
    @pml.machine_functions.each do |fun|
      # address of function
      addr = fun.address
      abs_instr_index = 0
      call_return_instr = {}

      # for all basic blocks
      fun.blocks.each do |block|

        # blocks that consist of labels only (used in some benchmarks for flow facts)
        if block.empty?
          (@empty_blocks[block.address]||=[]).push(block)
          next
        end

        # generate basic block event at first instruction
        add_watch(@wp_block_start, block.address, block)

        # generate return event at return instruction
        # FIXME: does not work for predicated return instructions now,
        # it would be helpful if return instructions where marked in PML
        if block.successors.empty?
          return_ins = block.instructions[-1 - @delay_slots]
          add_watch(@wp_return_instr,return_ins['address'],return_ins)
        end

        block.instructions.each do |instruction|
          if call_return_instr[abs_instr_index]
            add_watch(@wp_callreturn_instr,instruction['address'],instruction)
          end
          if ! instruction.callees.empty?
            add_watch(@wp_call_instr,instruction['address'],instruction)
            call_return_instr[abs_instr_index + 1 +@delay_slots]=true
          end
          abs_instr_index += 1
        end
      end
    end
  end
  def add_watch(dict, addr, data)
    if ! addr
      warn ("No address for #{data.inspect[0..60]}")
    elsif dict[addr]
      raise Exception.new("Duplicate watchpoint at address #{addr.inspect}: #{data} already set to #{dict[addr]}")
    else
      @wp[addr] = true
      dict[addr] = data
    end
  end
end

# Recorder which just dumps event to the given stream
class VerboseRecorder
  def initialize(out=$>)
    @out = out
  end
  def method_missing(event, *args)
    @out.puts("EVENT #{event.to_s.ljust(15)} #{args.join(" ")}")
  end
end

# Recorder for frequencies relative to a global function scope
class GlobalRecorder
  attr_reader :results
  def initialize(start_mf)
    @start_name = start_mf.name
    @results = FrequencyRecord.new("GlobalRecorder(#{start_mf})")
  end
  def function(callee,callsite,cycles)
    if callee['name']==@start_name
      @running = true
      results.start(cycles)
    end
    results.call(callsite,callee)
  end
  def block(mbb, _)
    return unless @running
    results.increment(mbb)
  end
  def ret(rsite,csite,cycles)
    if(rsite.function.name==@start_name)
      results.stop(cycles)
      @running = false
    end
  end
  def eof ; end
  def method_missing(event, *args); end
end

# Recorder for function-local block frequencies
class LocalRecorder
  attr_reader :results
  def initialize(start_mf)
    @start_name = start_mf.name
    @running = false
    @results = {}
  end
  def function(callee,callsite,cycles)
    @running = true if callee['name']==@start_name
    return unless @running
    @function = callee
    results[@function] = FrequencyRecord.new("LocalRecorder(#{callee})") unless results[@function]
    results[@function].start(cycles)
  end
  def block(mbb, _)
    return unless @running
    results[@function].increment(mbb)
  end
  def ret(rsite,csite,cycles)
    return unless @running
    assert("Bad function context: #{@function}") { @function == rsite.function && @results[@function] }
    @results[@function].stop(cycles)
    @running = false if rsite.function.name==@start_name
    @function = csite.function if @running
  end
  def eof ; end
  def method_missing(event, *args); end
end

# Recorder for local loop frequencies
class LoopRecorder
  attr_reader :results
  def initialize(start_mf)
    @start_name = start_mf.name
    @started = false
    @results = {}
  end
  def function(callee,callsite,cycles)
    @started = true if callee['name']==@start_name
  end
  def ret(rsite,csite,cycles)
    @started = false if rsite.function.name==@start_name
  end
  def loopenter(bb, cycles)
    return unless @started
    results[bb] = FrequencyRecord.new("LoopRecorder(#{bb})") unless results[bb]
    results[bb].start(cycles)
    results[bb].increment(bb)
  end
  def loopcont(bb, _)
    return unless @started
    results[bb].increment(bb)
  end
  def loopexit(bb, cycles)
    return unless @started
    results[bb].stop(cycles)
  end
  def eof ; end
  def method_missing(event, *args); end
end

# Utility class to record frequencies when analyzing traces
class FrequencyRecord
  attr_reader :name, :runs, :cycles, :freqs, :calltargets
  def initialize(name)
    @name = name
    @runs = 0
    @calltargets = {}
  end
  def start(cycles)
    @cycles_start = cycles
    @runs += 1
    @current_record = Hash.new(0)
  end
  def increment(bb)
    @current_record[bb] += 1 if @current_record
  end
  def to_s
    "FrequencyRecord{ name = #{@name} }"
  end
  def call(callsite,callee)
    (@calltargets[callsite]||=Set.new).add(callee) if @current_record && callsite
  end
  def stop(cycles)
    die "Recorder: stop without start: #{@name}" unless @current_record
    @cycles = merge_ranges(cycles - @cycles_start, @cycles)
    unless @freqs
      @freqs = {}
      @current_record.each do |bref,count|
        @freqs[bref] = count .. count
      end
    else
      @current_record.each do |bref,count|
        if ! @freqs.include?(bref)
          @freqs[bref] = 0 .. count
        else
          @freqs[bref] = merge_ranges(count, @freqs[bref])
        end
      end
      @freqs.each do |bref,count|
        @freqs[bref] = merge_ranges(count, 0..0) unless @current_record.include?(bref)
      end
    end
    @current_record = nil
  end
  def dump(io=$>, header=nil)
    (io.puts "No records";return) unless @freqs
    io.puts "---"
    io.puts header if header
    io.puts "cycles: #{cycles}"
    @freqs.keys.sort.each do |bref|
      io.puts "  #{bref.to_s.ljust(15)} \\in #{@freqs[bref]}"
    end
    @calltargets.each do |site,recv|
      io.puts "  #{site} calls #{recv.to_a.join(", ")}"
    end
  end
end

# Records progress node trace
class ProgressTraceRecorder
  attr_reader :level, :trace, :internal_preds
  def initialize(pml, entry, is_machine_code, options)
    @pml, @options = pml, options
    @trace, @entry, @level = [], entry, is_machine_code ? :dst : :src
    @internal_preds, @pred_list = [], []
    @callstack = []
  end
  # set current relation graph
  # if there is no relation graph, skip function
  def function(callee,callsite,cycles)
    @rg = @pml.relation_graphs.by_name(callee.name, @level)
    $dbgs.puts "Call to rg for #{@level}-#{callee}: #{@rg.nodes.first}" if @rg && @options.debug
    @callstack.push(@node)
    @node = nil
  end
  # follow relation graph, emit progress nodes
  def block(bb, _)
    return unless @rg
    if ! @node
      first_node = @rg.nodes.first
      assert("at_entry == at entry RG node") {
        first_node.type == :entry
      }
      assert("at_entry == at first block") {
        bb == first_node.get_block(level)
      }
      @node = first_node
      $dbgs.puts "Visiting first node: #{@node}" if @options.debug
      return
    end
    # find matching successor progress node
    succs = @node.successors_matching(bb, @level)
    if succs.length == 0
      raise Exception.new("progress trace: no matching successor")
    elsif succs.length > 1
      raise Exception.new("progress trace: indeterministic successor choice: #{@node} via #{bb}: #{succs}")
    else
      succ = succs.first
      if succ.type == :progress
        trace.push(succ)
        internal_preds.push(@pred_list)
        @pred_list = []
      else
        @pred_list.push(succ)
      end
      @node = succ
      $dbgs.puts "Visiting node: #{@node}" if @options.debug
    end
  end
  # set current relation graph
  def ret(rsite,csite,cycles)
    return if csite.nil?
    @rg = @pml.relation_graphs.by_name(csite.function.name, @level)
    @node = @callstack.pop
    $dbgs.puts "Return to rg for #{@level}-#{csite.function}: #{@rg.nodes.first}" if @rg and @options.debug
  end
  def eof ; end
  def method_missing(event, *args); end
end

end # module pml
