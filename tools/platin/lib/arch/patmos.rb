#
# PLATIN tool set
#
# Patmos specific functionality
#

#
# pasim traces
#

module Patmos

require "tempfile"

#
# Class to (lazily) read pasim simulator trace
# yields [program_counter, cycles] pairs
#
class SimulatorTrace
  attr_reader :elf, :arch, :stats_num_items
  def initialize(elf, arch, options, watchpoints)
    @elf, @arch, @options, @watchpoints = elf, arch, options, watchpoints
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
	wpfile = build_wp_file
        needs_options(@options, :pasim)
        pasim_options="--debug=0 --debug-fmt=trace -b #{@elf} --wpfile=#{wpfile.path}"
	pasim_options+=" -I #{@options.sim_input}" if @options.sim_input
	pasim_options+=" --flush-caches #{@options.sim_flush_caches}" if @options.sim_flush_caches
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
	wpfile.unlink unless @options.outdir
        if status == 127
          die("Running the simulator '#{@options.pasim}' failed: Program not found (exit status 127)")
        end
      end
    end
  end
  private
  def build_wp_file
    if @options.outdir
      file = File.open(File.join(@options.outdir, 'watchpoints.txt'), 'w')
    else
      file = Tempfile.new('wp')
    end
    @watchpoints.each { |wp,_|
      file.puts(wp.to_s)
    }
    file.close
    file
  end
  def parse(line)
    return nil unless line and not line.chomp.empty?
    pc, cyc, instr = line.split(' ',3)
    begin
      [ Integer("0x#{pc}"), Integer(cyc), Integer(instr) ]
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
  def Architecture.default_instr_cache(type)
    if type == 'method-cache'
      PML::CacheConfig.new('method-cache','method-cache','fifo',16,8,4096)
    else
      PML::CacheConfig.new('instruction-cache','instruction-cache','dm',1,16,4096)
    end
  end
  def Architecture.default_config
    memories = PML::MemoryConfigList.new([PML::MemoryConfig.new('main','simple',nil,2*1024*1024,16,0,21,0,21)])
    caches = PML::CacheConfigList.new([Architecture.default_instr_cache('method-cache'),
                                  PML::CacheConfig.new('stack-cache','stack-cache','block',nil,4,2048),
                                  PML::CacheConfig.new('data-cache','set-associative','dm',nil,16,2048) ])
    full_range = PML::ValueRange.new(0,0xFFFFFFFF,nil)
    memory_areas =
      PML::MemoryAreaList.new([PML::MemoryArea.new('code','code',caches.list[0], memories.first, full_range),
                               PML::MemoryArea.new('data','data',caches.list[2], memories.first, full_range) ])
    PML::MachineConfig.new(memories,caches,memory_areas)
  end

  def simulator_trace(options, watchpoints)
    SimulatorTrace.new(options.binary_file, self, options, watchpoints)
  end

  def return_stall_cycles(ret_instruction, ret_latency)
    ret_latency - 1
  end

  def num_slots
    2
  end

  # The number of bytes fetched per cycle
  def fetch_size
    8
  end

  # Number of bytes prependend to each subfunction
  def subfunction_prefix_size
    4
  end

  def main_memory(area_name)
    if area = @config.memory_areas.by_name(area_name)
      area.memory
    else
      @config.main_memory
    end
  end

  def method_cache
    mc = @config.caches.by_name('method-cache')
    return nil if mc.nil? or mc.type == 'none'
    mc
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

  def code_memory
    cm = @config.memory_areas.by_name('code')
    cm.memory if cm
  end

  def data_memory
    dm = @config.memory_areas.by_name('data')
    dm.memory if dm
  end

  def local_memory
    # used for local scratchpad and stack cache accesses
    @config.memories.by_name("local")
  end

  def stack_cache
    sc = @config.caches.by_name('stack-cache')
    return nil if sc.nil? or sc.type == 'none'
    sc
  end

  def data_cache
    # TODO check if this is consistent with what is configured in the 
    #      data memory-area (but check only once!)
    dc = @config.caches.by_name('data-cache')
    return nil if dc.nil? or dc.type == 'none'
    dc
  end

  def instruction_cache
    ic = @config.caches.by_name('instruction-cache')
    return nil if ic.nil? or ic.type == 'none'
    ic
  end

  def instruction_fetch_bytes
    num_slots * 4
  end

  # Return the maximum size of a load or store in bytes.
  def max_data_transfer_bytes
    4
  end

  # Check if this instruction accesses the data cache.
  # TODO This check is used to determine if the access should be handled
  #      by the generic DataCache analysis. We use the DataCache analysis
  #      to attach costs to cached and bypass loads/stores, but we skip
  #      stack and local accesses. For now we assume that stack and local
  #      accesses always have zero latency. 
  #      We should instantiate a separate DataCacheAnalysis that uses the
  #      memory named "local" and a different line-size (4) handles
  #      all local memory accesses.
  def data_cache_access?(instr)
    instr.memtype == "cache" || instr.memtype == "memory"
  end

  def local_access?(instr)
    instr.memtype == "local" || instr.memtype == "stack"
  end

  def memory_access?(instr)
    instr.memtype == "cache" || instr.memtype == "memory" || %w[SRESi SENSi].include?(instr.opcode)
  end

  def path_bcet(ilist)
    cost = ilist.reduce(0) do |cycles, instr|
      cycles + (instr.bundled? ? 0 : 1)
    end
    cost
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
      patmos_options = REXML::Element.new("patmos_options")
      # workaround sets SC size and base address to 0
      if options.ait_disable_internal_sc
        sc_option = REXML::Element.new("stack_cache_base")
        sc_option << REXML::Text.new("0x0")
        patmos_options << sc_option
        sc_size = "0x0"
      else
        sc_size = sprintf("0x%x", sc.size)
      end
      sc_option = REXML::Element.new("stack_cache_size")
      sc_option << REXML::Text.new(sc_size)
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
      if main_memory.kind
        opts.push("--gkind")
        opts.push(main_memory.kind)
      end
      if main_memory.ramul_config
        opts.push("--ramul-config")
        opts.push(main_memory.ramul_config)
      end
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
	cache.policy.downcase
      elsif not cache.fully_assoc?
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
      # an ideal memory; bypasses still have a latency.
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

  # Update the configuration using the given options
  def update_cache_config(options)
   
   # Update data cache
    if options.data_cache_size or options.data_cache_policy
      dc_policy = options.data_cache_policy[:policy] if options.data_cache_policy
      dc_assoc  = options.data_cache_policy[:assoc]  if options.data_cache_policy
      # We are not using self.data_cache here because it would return
      # null if the type is set to 'none', even if the entry exists.
      dc = @config.caches.by_name('data-cache')
      if dc.nil? and (dc_policy != "no")
        dc = self.class.default_config.caches.by_name('data-cache')
	@config.caches.add(dc)
      end
      if dc
	dc.size = options.data_cache_size if options.data_cache_size
	# We are not deleting the cache entry here, partly because it
	# allows the user to easily edit the generated config manually, 
	# partly because it is easier to implement.
	dc.type = 'set-associative' if dc_policy
	dc.type = 'none' if dc_policy == 'no'
	dc.policy = dc_policy if dc_policy and dc_policy != 'no'
	# Set the associativiy whenever we set a policy
	dc.associativity = dc_assoc if dc_policy
	
	# Update the data memory area
	dma = @config.memory_areas.by_name('data')
	dma.cache = (dc.type != 'none' ? dc : nil) if dma
      end
    end
   
   # Update stack cache
    if options.stack_cache_size or options.stack_cache_type
      # We are not using self.stack_cache here because it would return
      # null if the type is set to 'none', even if the entry exists.
      sc = @config.caches.by_name('stack-cache')
      if dc.nil? and options.stack_cache_type != 'no'
	sc = self.class.default_config.caches.by_name('stack-cache')
	@config.caches.add(sc)
      end
      if sc
	sc.size = options.stack_cache_size if options.stack_cache_size
	sc.type = 'stack-cache' if options.stack_cache_type
	sc.type = 'none' if options.stack_cache_type == 'no'
	sc.policy = options.stack_cache_type if options.stack_cache_type and options.stack_cache_type != 'no'
      end
    end
   
   # Update instruction cache / method cache
    if options.instr_cache_kind or options.instr_cache_size or options.instr_cache_policy or options.instr_cache_line_size
      ic_policy = options.instr_cache_policy[:policy] if options.instr_cache_policy
      ic_assoc  = options.instr_cache_policy[:assoc]  if options.instr_cache_policy
      if options.instr_cache_kind
        ic_key = options.instr_cache_kind == 'icache' ? "instruction-cache" : "method-cache"
      elsif instruction_cache
	ic_key = "instruction-cache"
      else
	ic_key = "method-cache"
      end
      # Get or create the active cache
      ic = @config.caches.by_name(ic_key)
      if ic.nil? and ic_policy != 'no'
	ic = self.class.default_instr_cache(ic_key)
	@config.caches.add(ic)
      end
      # Deactivate the other instruction caches
      @config.caches.each { |cache|
        next if cache.name == ic_key
	cache.type = 'none' if ['instruction-cache','method-cache'].include?(cache.name)
      }
      if ic
	ic.size = options.instr_cache_size if options.instr_cache_size
	# We are not deleting the cache entry here, partly because it
	# allows the user to easily edit the generated config manually, 
	# partly because it is easier to implement.
	ic.type = (ic_key == 'method-cache' ? 'method-cache' : 'set-associative') if ic_policy
	ic.type = 'none' if ic_policy == 'no'
	ic.policy = ic_policy if ic_policy and ic_policy != 'no'
	# Set the associativiy whenever we set a policy
	ic.associativity = ic_assoc if ic_policy
      
	# Update the code memory area
        cma = @config.memory_areas.by_name('code')
        cma.cache = (ic.type != 'none' ? ic : nil) if cma
      end
    end
  end

  def update_heap_symbols(stack_size, num_stacks)
    dma = @config.memory_areas.by_name('data')
    return unless dma

    memsize = dma.memory.size

    dma.set_attribute('stack-base', memsize)
    dma.set_attribute('shadow-stack-base', memsize - stack_size)
    dma.set_attribute('heap-end', memsize - stack_size * num_stacks * 2)
  end

end

end # module patmos

# Extend PML
module PML

# Register architecture
Architecture.register("patmos", Patmos::Architecture)

end # module PML
