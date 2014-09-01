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
        pasim_options="--debug=0 --debug-fmt=trace -b #{@elf}"
        cmd = "#{@options.pasim} #{arch.config_for_simulator.join(" ")} #{pasim_options} 2>&1 1>/dev/null"
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
    return nil unless line and not line.chomp.empty?
    pc, cyc = line.split(' ',2)
    begin
      [ Integer("0x#{pc}"), Integer(cyc) ]
    rescue Exception => e
      raise Exception.new("Patmos::SimulatorTrace: bad line (\"#{line.chomp}\")")
    end
  end
end

class Architecture < PML::Architecture
  attr_reader :triple, :config
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
    memories = PML::MemoryConfigList.new([PML::MemoryConfig.new('main',64*1024*1024,8,0,0,0,0)])
    caches = PML::CacheConfigList.new([PML::CacheConfig.new('method-cache','method-cache','fifo',16,8,4096),
                                  PML::CacheConfig.new('stack-cache','stack-cache','block',nil,4,1024),
                                  PML::CacheConfig.new('data-cache','set-associative','lru',4,32,1024) ])
    full_range = PML::ValueRange.new(0,0xFFFFFFFF,nil)
    memory_areas =
      PML::MemoryAreaList.new([PML::MemoryArea.new('code','code',caches.list[0], memories.first, full_range),
                               PML::MemoryArea.new('data','data',caches.list[2], memories.first, full_range) ])
    PML::MachineConfig.new(memories,caches,memory_areas)
  end

  def simulator_trace(options)
    SimulatorTrace.new(options.binary_file, self, options)
  end

  def num_slots
    2
  end

  def method_cache
    @config.caches.by_name('method-cache')
  end

  #
  # For a subfunction, we need to load the header (one word
  # that contains the subfunction's size) plus the subfunction
  # Additionally, we need to consider the alignment of memory
  # transfers
  def subfunction_miss_cost(sf)
    memory = @config.memory_areas.by_name('code').memory
    memory.read_delay(sf.entry.address - 4, sf.size + 4)
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

  def instruction_fetch_bytes
    num_slots * 4
  end

  def path_wcet(ilist)
    # puts ilist.first.inspect unless ilist.empty?
    cost = ilist.reduce(0) do |cycles, instr|
      flushes = 0
      if instr.delay_slots == 0
        if instr.branch_type == 'call'
          flushes = 3
        end
      end
      cycles + (instr.bundled? ? 0 : 1) + flushes
    end
    cost
  end

  def edge_wcet(ilist,index,edge)
    cost = 0
    if index
      instr = ilist[index]
      if instr.delay_slots == 0
        if instr.branch_type == 'return' || instr.opcode.start_with?('BRCF')
          cost = 3
        else
          cost = 2
        end
      end
    end
    cost
  end

  def config_for_apx(options)
    if sc = stack_cache
      sc_size = sprintf("0x%x", sc.size)
      sc_option = REXML::Element.new("stack_cache_size")
      sc_option << REXML::Text.new(sc_size)
      patmos_options = REXML::Element.new("patmos_options")
      patmos_options << sc_option
      patmos_options
    else
      nil
    end
  end

  # Options (as of 2013/10/16):
  #
  #  -mpatmos-disable-function-splitter
  #    => should be disabled for instruction cache
  #  -mpatmos-method-cache-size
  #  -mpatmos-preferred-subfunction-size
  #  -mpatmos-max-subfunction-size
  #    => needs to be equal to block size for fixed-block method cache
  #  -mpatmos-subfunction-align
  #  -mpatmos-basicblock-align
  #    => should be at least 8 bytes for set-associative caches

  def config_for_clang(options)
    opts = []
    if mc = method_cache
      opts.push("-mpatmos-method-cache-size=#{mc.size}")
      if pref_sf_size = method_cache.get_attribute("preferred-subfunction-size")
        opts.push("-mpatmos-preferred-subfunction-size=#{pref_sf_size}")
      end
      if max_sf_size = method_cache.get_attribute("max-subfunction-size")
        opts.push("-mpatmos-max-subfunction-size=#{max_sf_size}")
      end
    else
      opts.push("-mpatmos-disable-function-splitter")
      # opts.push("-mpatmos-basicblock-align=8")
    end
    if sc = stack_cache
      if options.sca
        opts.push("-mpatmos-enable-stack-cache-analysis")
        # we need to specify a solver
        opts.push("-mpatmos-ilp-solver=#{options.sca['solver']}")
        # we need to specify a recursion.lp file
        opts.push("-mpatmos-stack-cache-analysis-bounds=#{options.sca['bounds']}") unless options.sca['bounds'].empty?
      end
      opts.push("-mpatmos-stack-cache-block-size=#{sc.block_size}")
      opts.push("-mpatmos-stack-cache-size=#{sc.size}")
      if sc.policy == 'ablock'
        opts.push("-mpatmos-enable-block-aligned-stack-cache")
      end
    else
      opts.push("-mpatmos-disable-stack-cache")
    end
    @config.memory_areas.each { |c|
      heap_end = c.get_attribute('heap-end')
      opts.push("-mpatmos-heap-end=#{heap_end}") if heap_end
      stack_base = c.get_attribute('stack-base')
      opts.push("-mpatmos-stack-base=#{stack_base}") if stack_base
      shadow_stack_base = c.get_attribute('shadow-stack-base')
      opts.push("-mpatmos-shadow-stack-base=#{shadow_stack_base}") if shadow_stack_base
    }
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

    def get_cache_kind(cache)
      if cache.policy && [ "dm", "ideal", "no" ].include?(cache.policy.downcase)
        # Ignore associativity here
	dc.policy.downcase
      elsif cache.associativity && cache.associativity.to_i >= 1
        if cache.policy && cache.policy.downcase == 'lru'
          "lru#{cache.associativity}"
        elsif cache.policy && cache.policy.downcase == 'fifo'
          "fifo#{cache.associativity}"
        else
          warn("Patmos simulator configuration: the only supported cache replacement "+
               "policies with associativity >= 1 are LRU and FIFO")
	  "no"
        end
      elsif cache.policy && [ "lru", "fifo" ].include?(cache.policy.downcase)
        # If no associativity is given, use fully-associative caches
	cache.policy.downcase
      else
        "no"
      end
    end

    if dc = data_cache
      opts.push("--dcsize")
      opts.push(dc.size)
      opts.push("--dlsize")
      opts.push(dc.block_size)
      opts.push("--dckind")
      # Note: 'ideal' is not the same as mapping all data accesses to 
      # an ideal memory; bypasses and stores still have a latency.
      opts.push( get_cache_kind(dc) )
    else
      # if data is mapped to single-cycle access memory,
      # use an 'ideal' data cache
      data_area = @config.memory_areas.by_name('data')
      if data_area.memory.ideal?
        # FIXME This is incorrect for bypasses, but simulator does
	# not support different timings based on access type yet.
	warn("Bypass data loads and data stores are configured to be single cycle, but this "+
	     "is not supported by pasim at the moment.")
        opts.push("--dckind")
        opts.push("ideal")
      else
	# In case no D$ is specified, disable the data cache
        opts.push("--dckind")
        opts.push("no")
      end
    end
    if sc = stack_cache
      warn("Cache named 'stack-cache' is not of type 'stack-cache'") unless sc.type == "stack-cache"
      opts.push("--scsize")
      opts.push(sc.size)
      opts.push("--sckind")
      opts.push(sc.policy || "block")
    else
      # no stack cache specified -> default to data cache
      opts.push("--sckind")
      opts.push("dcache")
    end
    if mc = method_cache
      warn("Cache named 'method-cache' is not of type 'method-cache'") unless mc.type == "method-cache"
      opts.push("--icache")
      opts.push("mcache")
      opts.push("--mcsize")
      opts.push(mc.size)
      opts.push("--mbsize")
      opts.push(mc.block_size)
      opts.push("--mcmethods")
      opts.push(mc.associativity)
      opts.push("--mckind")
      opts.push((mc.policy || "fifo").downcase)
    elsif ic = instruction_cache
      warn("Cache named 'instruction-cache' must not be of type 'method-cache'") if ic.type == "method-cache"
      opts.push("--icache")
      opts.push("icache")
      opts.push("--mcsize")
      opts.push(ic.size)
      opts.push("--ilsize")
      opts.push(ic.block_size)
      opts.push("--ickind")
      opts.push( get_cache_kind(ic) )
    else
      # if code is mapped to single-cycle access memory,
      # use an 'ideal' instruction cache
      code_area = @config.memory_areas.by_name('code')
      if code_area.memory.ideal?
        opts.push("--icache")
        opts.push("icache")
        opts.push("--ickind")
        opts.push("ideal")
      else
        die("Patmos simulator configuration: underspecified I$")
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
