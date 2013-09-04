#
# PLATIN tool set
#
# Patmos specific functionality
#

#
# pasim traces
#

module Patmos

#
# Class to (lazily) read pasim simulator trace
# yields [program_counter, cycles] pairs
#
class SimulatorTrace
  attr_reader :elf, :arch, :stats_num_items
  def initialize(elf, arch, options)
    @elf, @arch, @options = elf, arch, options
    @stats_num_items = 0
  end
  def each
    die("File '#{@elf}' (ELF) not found") unless File.exist?("#{@elf}")
    if @options.trace_file
      fh = $stdin
      begin
        if @options.trace_file[-3..-1] == '.gz'
          require 'zlib'
          fh = Zlib::GzipReader.open(@options.trace_file)
        elsif @options.trace_file != '-'
          fh = File.open(@options.trace_file, "r")
        end
        fh.each_line { |line|
          yield parse(line)
          @stats_num_items += 1
        }
      ensure
        fh.close
      end
    else
      begin
        needs_options(@options, :pasim)
        cmd = "#{@options.pasim} #{arch.config_for_simulator.join(" ")} -q --debug 0 --debug-fmt trace -b #{@elf} 2>&1 1>/dev/null"
        debug(@options, :patmos) { "Running pasim: #{cmd}" }
        IO.popen("#{cmd}") do |io|
          while item=parse(io.gets)
            yield item
            @stats_num_items+=1
          end
        end
      ensure
        status = $?.exitstatus
        if status == 127
          die("Running the simulator '#{@options.pasim}' failed: Program not found (exit status 127)")
        end
      end
    end
  end
  private
  def parse(line)
    return nil unless line
    pc, cyc = line.split(' ',2)
    begin
      [ Integer("0x#{pc}"), Integer(cyc) ]
    rescue Exception => e
      raise Exception.new("Patmos::SimulatorTrace: bad line #{line}")
    end
  end
end

class Architecture < PML::Architecture
  attr_reader :config
  def initialize(triple, config)
    @triple, @config = triple, config
    @config = self.class.default_config unless @config
  end
  def Architecture.simulator_options(opts)
    opts.on("--pasim-command FILE", "path to pasim (=pasim)") { |f| opts.options.pasim = f }
    opts.add_check do |options|
      options.pasim = "pasim" unless options.pasim || options.trace_file
    end
  end
  def Architecture.default_config
    memories = MemoryConfigList.new([MemoryConfig.new('main',64*1024*1024,8,0,0,0,0)])
    caches = CacheConfigList.new([CacheConfig.new('method-cache','method-cache','fifo',32,32,1024),
                                  CacheConfig.new('stack-cache','stack-cache','stack',nil,32,1024),
                                  CacheConfig.new('data-cache','set-associative','lru',4,32,1024) ])
    full_range = ValueRange.new(0,0xFFFFFFFF,nil)
    memory_areas = MemoryAreaList.new([MemoryArea.new('code','code',caches.list[0], memories.first, full_range),
                                       MemoryArea.new('data','data',caches.list[2], memories.first, full_range) ])
    MachineConfig.new(memories,caches,memory_areas)
  end
  def simulator_trace(options)
    SimulatorTrace.new(options.binary_file, self, options)
  end

  def method_cache
    @config.caches.by_name('method-cache')
  end
  def stack_cache
    @config.caches.by_name('stack-cache')
  end
  def data_cache
    @config.caches.by_name('data-cache')
  end
  def instruction_cache
    @config.caches.by_name('instruction-cache')
  end

  def config_for_clang
    opts = []
    if mc = method_cache
      opts.push("-mpatmos-method-cache-block-size=#{mc.block_size}")
      opts.push("-mpatmos-method-cache-size=#{mc.size}")
    else
      # XXX: no option to disable method cache for patmos-clang?
    end
    if sc = stack_cache
      # does not work properly at the moment
      #opts.push("-mpatmos-enable-stack-cache-analysis")
      #opts.push("-mpatmos-stack-cache-analysis-bounds=recursion.lp")
      #opts.push("-mpatmos-stack-cache-block-size=#{sc.block_size}")
      #opts.push("-mpatmos-stack-cache-size=#{sc.size}")
    else
      opts.push("-mpatmos-disable-stack-cache")
    end
    opts
  end

  def config_for_simulator
    opts = []

    if main_memory = @config.main_memory
      if main_memory.size
        opts.push("--gsize")
        opts.push(main_memory.size)
      end
      if main_memory.transfer_size
        opts.push("--bsize")
        opts.push(main_memory.transfer_size)
      end
      if main_memory.read_latency && main_memory.read_transfer_time
        opts.push("--tdelay")
        opts.push(main_memory.read_latency)
        opts.push("--gtime")
        opts.push(main_memory.read_transfer_time)
      end
    end

    # ??
    #   -p [ --posted ] arg (=0)  Enable posted writes (sets max queue size)
    #   -l [ --lsize ] arg (=2k)  local memory size in bytes

    if dc = data_cache
      opts.push("--dcsize")
      opts.push(dc.size)
      opts.push("--dlsize")
      opts.push(dc.block_size)
      opts.push("--dckind")
      if dc.associativity.to_i > 1
        if dc.policy && dc.policy.downcase != 'lru'
          warn("Patmos simulator configuration: the only supported replacement "+
               "policy for data cache simulation is LRU")
        end
        opts.push("lru#{dc.associativity}")
      else
        opts.push("no")
      end
    end
    if sc = stack_cache
      opts.push("--scsize")
      opts.push(sc.size)
      opts.push("--sbsize")
      opts.push(sc.block_size)
      opts.push("--sckind")
      opts.push("block")
    end
    if mc = method_cache
      opts.push("--mcsize")
      opts.push(mc.size)
      opts.push("--mbsize")
      opts.push(mc.block_size)
      opts.push("--mckind")
      opts.push((mc.policy || "fifo").downcase)
    elsif ic = instruction_cache
      opts.push("--icache")
      opts.push("--mcsize")
      opts.push(ic.size)
      opts.push("--ilsize")
      opts.push(ic.block_size)
      opts.push("--ickind")
      if ic.associativity.to_i > 1
        if ic.policy && ic.policy.downcase != 'lru'
          warn("Patmos simulator configuration: the only supported replacement "+
               "policy for data cache simulation is LRU")
        end
        opts.push("lru#{ic.associativity}")
      else
        opts.push("no")
      end
    end
    opts
  end

end

end # module patmos

# Extend PML
module PML

# Register architecture
Architecture.register("patmos", Patmos::Architecture)

end # module PML
