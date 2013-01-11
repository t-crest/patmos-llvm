require File.join(File.dirname(__FILE__),"utils.rb")
include PMLUtils

# Class to (lazily) read pasim simulator trace
class SimulatorTrace
  def initialize(elf)
    @elf = elf
  end
  def each
    IO.popen("pasim -q --debug 0 --debug-fmt trace -b #{@elf} 2>&1 1>/dev/null") do |io|
      while item=parse(io.gets) ; yield item ; end
    end
  end
  def parse(line)
    return nil unless line
    pc, cyc = line.split(' ',2)
    [ Integer("0x#{pc}"), Integer(cyc) ]
  end
end

# Class to monitor trace and generate events
class TraceMonitor
  DELAY_SLOTS = 2
  def initialize(elf,pml,program_start = "main")
    @pml = pml
    @trace = SimulatorTrace.new(elf)
    @start = @pml.mf_mapping_to(program_start)['blocks'].first['address']
    @observers = []
    # whether an instruction is a watch point
    @wp = {}
    # function entry watch points
    @wp_fun_entry = {}
    # basic block watch points
    @wp_block_start = {}
    # call instruction watch points
    @wp_call_instr = {}
    # instructions which the callee returns to
    @wp_callreturn_instr = {}
    # return instruction watch points
    @wp_return_instr = {}
    build_watchpoints
  end
  def subscribe(obj)
    @observers.push(obj)
  end
  def run
    callstack, loopstack = [], []
    @trace.each do |pc,cycles|
      @started = true if pc == @start
      next unless @started
      next unless @wp[pc]
      if f = @wp_fun_entry[pc]
        loopstack = []
        publish(:function, f, callstack[-1], cycles)
      end
      if b = @wp_block_start[pc]
        while (b['loops']||[]).length < loopstack.length
          publish(:loopexit, loopstack.pop, cycles)
        end
        if b['loops'] && b['loops'].first == b['name']
          if b['loops'].length == loopstack.length
            publish(:loopcont, b, cycles)
          else
            loopstack.push b
            publish(:loopenter, b, cycles)
          end
        end
        publish(:block, b, cycles)
      end
      if c  = @wp_call_instr[pc]
        callstack.push(loopstack)
        callstack.push(c)
      end
      if cr = @wp_callreturn_instr[pc]
        callstack.pop
        loopstack = callstack.pop
      end
      if r = @wp_return_instr[pc]
        if callstack.empty?
          publish(:ret, r, nil, cycles)
          return
        end
        publish(:ret, r, callstack[-1], cycles)
      end
    end
  end
  def publish(msg,*args)
    @observers.each do |obs|
      obs.send(msg,*args)
    end
  end
  private
  def build_watchpoints
    # generate watchpoints for all relevant machine functions
    @pml['machine-functions'].each do |mf|
      fun = FunctionRef.new(mf)
      # address of function
      addr = fun.address
      # generate function entry event at first instruction
      add_watch(@wp_fun_entry,addr,FunctionRef.new(mf))

      abs_instr_index = 0
      call_return_instr = {}
      # for all basic blocks
      mf['blocks'].each do |mbb|
        block = BlockRef.new(mf,mbb)

        # generate basic block event at first instruction
        add_watch(@wp_block_start, block.address, block)

        # generate return event at return instruction
        # FIXME: does not work for predicated return instructions now,
        # it would be helpful if return instructions where dedicated
        if (block['successors']||[]).empty?
          rins = InsRef.new(mf,mbb,mbb['instructions'][-1-DELAY_SLOTS])
          add_watch(@wp_return_instr,rins['address'],rins)
        end

        mbb['instructions'].each do |ins|
          instruction = InsRef.new(mf,mbb,ins)
          if call_return_instr[abs_instr_index]
            add_watch(@wp_callreturn_instr,ins['address'],instruction)
          end
          if ! (ins['callees']||[]).empty?
            add_watch(@wp_call_instr,ins['address'],instruction)
            call_return_instr[abs_instr_index+1+DELAY_SLOTS]=true
          end
          abs_instr_index += 1
        end
      end
    end
  end
  def add_watch(dict,addr,data)
    if ! addr
      warn ("No address for #{data.inspect[0..60]}")
    elsif dict[addr]
      raise Exception.new("Duplicate watchpoint at address #{addr.inspect}")
    else
      @wp[addr] = true
      dict[addr] = data
    end
  end
end

class VerboseRecorder
  def initialize(out=$>)
    @out = out
  end
  def method_missing(event, *args)
    @out.puts("EVENT #{event.to_s.ljust(15)} #{args.join(" ")}")
  end
end

class GlobalRecorder
  attr_reader :results
  def initialize(start_mf)
    @start_name = start_mf['name']
    @results = FrequencyRecord.new
  end
  def function(callee,callsite,cycles)
    results.start(cycles) if callee['name']==@start_name
    results.call(callsite,callee)
  end
  def block(mbb, _)
    results.increment(mbb)
  end
  def ret(rsite,csite,cycles)
    results.stop(cycles) if(rsite.fref['name']==@start_name)
  end
  def method_missing(event,*args) ; end
end
class LoopRecorder
  attr_reader :results
  def initialize(start_mf)
    @start_name = start_mf['name']
    @started = false
    @results = {}
  end
  def function(callee,callsite,cycles)
    @started = true if callee['name']==@start_name
  end
  def ret(rsite,csite,cycles)
    @started = false if rsite.fref['name']==@start_name
  end
  def loopenter(bb, cycles)
    results[bb] = FrequencyRecord.new unless results[bb]
    results[bb].start(cycles)
    results[bb].increment(bb)
  end
  def loopcont(bb, _)
    results[bb].increment(bb)
  end
  def loopexit(bb, cycles)
    results[bb].stop(cycles)
  end
  def method_missing(event,*args) ; end
end

class AnalyzeTraceTool
  def AnalyzeTraceTool.run(elf,pml,options)
    tm = TraceMonitor.new(elf,pml)
    tm.subscribe(VerboseRecorder.new) if options.debug
    globalscope = pml.mf_mapping_to('main')
    global = GlobalRecorder.new(globalscope)
    loops  = LoopRecorder.new(globalscope)
    tm.subscribe(global)
    tm.subscribe(loops)
    tm.run

    if options.verbose
      puts "Global Frequencies"
      global.results.dump
      puts "Loop Bounds"
      loops.results.values.each { |r| r.dump }
    end
    data = pml.data
    data['flowfacts'] ||= []
    ffinit = { 'level' => 'machinecode', 'origin' => 'trace'}
    global.results.freqs.each do |block,freq|
      flowfact = FlowFact.new(ffinit)
      flowfact['scope'] = FlowFact.function(globalscope)
      flowfact['op'] = 'less-equal'
      flowfact['rhs'] = freq.max
      flowfact['classification'] = 'block-global'
      flowfact.add_term(FlowFact.block(block))
      data['flowfacts'].push(flowfact.data)
    end
    global.results.calltargets.each do |cs,receiverset|
      next unless cs['callees'].include?('__any__')
      flowfact = FlowFact.new(ffinit)
      flowfact['scope'] = FlowFact.function(globalscope)
      flowfact['op'] = 'equal'
      flowfact['rhs'] = 0
      flowfact['classification'] = 'calltargets-global'
      flowfact.add_term(FlowFact.instruction(cs), -1)
      receiverset.each do |fref| 
        flowfact.add_term(FlowFact.function(fref), 1)
      end
      data['flowfacts'].push(flowfact.data)
    end
    loops.results.values.each do |loopbound|
      loop,freq = loopbound.freqs.to_a[0]
      flowfact = FlowFact.new(ffinit)
      flowfact['scope'] = FlowFact.loop(loop)
      flowfact['op'] = 'less-equal'
      flowfact['rhs'] = freq.max
      flowfact['classification'] = 'loop-local'
      flowfact.add_term(FlowFact.block(loop))
      data['flowfacts'].push(flowfact.data)
    end
    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF
Generate flow facts reflecting frequencies from machine-code
execution traces generated with 'pasim --debug'.
Also adds observed receivers to indirect calls callee field.
EOF

  options, args, pml = PML::optparse(1..1, "program.elf", SYNOPSIS, :type => :io) { }
  AnalyzeTraceTool.run(args.first, pml, options).dump_to_file(options.output)
end
