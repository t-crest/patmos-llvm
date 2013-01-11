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
  def initialize(elf,pml,program_start = "main")
    @pml = pml
    @trace = SimulatorTrace.new(elf)
    @start = @pml.mf_mapping_to(program_start)['blocks'].first['address']
    @observers = []
    build_watchpoints
  end
  def subscribe(obj)
    @observers.push(obj)
  end
  def run
    callstack = []
    @trace.each do |pc,cycles|
      @started = true if pc == @start
      next unless @started
      if f = @fpoints[pc]
        publish(:function, f, callstack[-1],cycles)
      end
      if b = @bpoints[pc]
        publish(:block, b, cycles)
      end
      if c = @cpoints[pc]
        callstack.push(c)
      end
      if r = @rpoints[pc]
        if callstack.empty?
          publish(:ret, r, nil, cycles)
          return
        end
        c = callstack.pop
        publish(:ret, r, c, cycles)
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
    @fpoints, @bpoints, @cpoints, @rpoints = {}, {}, {}, {}
    @pml['machine-functions'].each do |mf|
      addr = mf['blocks'].first['address']
      add_watch(@fpoints,addr,FunctionRef.new(mf))
      mf['blocks'].each do |mbb|
        block = BlockRef.new(mf,mbb)
        add_watch(@bpoints,mbb['address'],block)
        if (mbb['successors']||[]).empty?
          rins = InsRef.new(mf,mbb,mbb['instructions'].last)
          add_watch(@rpoints,rins['address'],rins)
        end
        mbb['instructions'].each do |ins|
          instruction = InsRef.new(mf,mbb,ins)
          unless (ins['callees']||[]).empty?
            add_watch(@cpoints,ins['address'],instruction)
          end
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
      dict[addr] = data
    end
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
end

class AnalyzeTraceTool
  def AnalyzeTraceTool.run(elf,pml)
    tm = TraceMonitor.new(elf,pml)
    globalscope = pml.mf_mapping_to('main')
    global = GlobalRecorder.new(globalscope)
    tm.subscribe(global)
    tm.run

    puts "Global Frequencies"
    global.results.dump

    data = pml.data
    data['flowfacts'] ||= []
    global.results.freqs.each do |block,freq|
      flowfact = { 'level' => 'machinecode', 'origin' => 'trace' }
      flowfact['scope'] = globalscope['name']
      pp = { 'function' => block.fref.name, 'block' => block.name }
      flowfact['lhs'] = [{ 'factor' => 1, 'program-point' => pp }]
      flowfact['op'] = 'less-equal'
      flowfact['rhs'] = freq.max
      data['flowfacts'].push(flowfact)
    end

    global.results.calltargets.each do |cs,receiverset|
      next unless cs['callees'].include?("__any__")
      cs.data['callees'] = (cs['callees'] + receiverset.map { |fref| fref.name }).uniq
    end
    pml
  end
end
