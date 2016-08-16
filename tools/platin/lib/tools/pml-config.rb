#
# platin toolchain: pml-config
#
# small tool to create a machine configuration
#
require 'platin'
require 'ext/ait'
include PML

class PMLConfigTool
  def PMLConfigTool.parse_size(size)
    if /^([0-9.]+) *([kmg]?)b?$/ =~ size.downcase
      size = $1.to_f
      case $2
      when "k" then size *= 1024
      when "m" then size *= 1024**2
      when "g" then size *= 1024**3
      end
    elsif /^0x[0-9a-fA-F]+$/ =~ size
      size = Integer(size)
    else
      die("Invalid size argument: #{size}")
    end
    size.to_i
  end
  def PMLConfigTool.parse_policy(policy)
    case policy
    when "ideal", "dm", "no"
      { policy: policy, assoc: nil }
    when /^(lru|fifo)(\d+)?$/
      { policy: $1, assoc: ($2 ? $2.to_i : nil) }
    else
      die("Unknown policy: #{policy}")
    end
  end
  def PMLConfigTool.parse_memory_kind(kind)
    case kind
    when "simple", "ddr3", "ddr4", "lpddr3", "lpddr4"
      kind
    else
      die("Unknown memory kind: #{kind}")
    end
  end

  def PMLConfigTool.add_options(opts)
    opts.on("--target TRIPLE", "Target architecture triple (required if no PML input is given)") { |a|
      opts.options.triple = a
    }

    # TODO Some (if not all) of the options here may be specific to an architecture. There are several ways to handle this
    #      - Let pml.arch define and check the options. This requires that at least the --target option is already
    #        parsed or the PML file is loaded, so that pml.arch is available. Problem: how do we handle 
    #        unknown options in the first pass?
    #      - Add all options to the option parser, but only include them in the help text and check them
    #        when an architecture is given, i.e. with 'pml-config --target patmos --help'. Problem: different
    #        architectures may have different definitions (and short names) of the same option name. Is this a problem?
    #      - Leave it like it is, with a common set of options for all architectures. This ensures consistency
    #        of the options across archs, but is less nice to maintain and it is very difficult to understand for the
    #        user, which options are actually used for a given architecture.
    #      - Make the options more generic to avoid architecture-specific options altogether.

    opts.on("--gkind KIND", "Kind of main memory (simple, ddr3, ddr4, lpddr3. lpddr4)") { |p|
      opts.options.memory_kind = parse_memory_kind(p)
    }
    opts.on("--ramul-config FILENAME", "Name of ramulator configuration file (requires non-simple gkind)") { |p|
      opts.options.ramul_config = p
    }
    opts.on("-g", "--gsize SIZE", "Global memory size") { |s|
      opts.options.memory_size = parse_size(s)
    }
    opts.on("-G", "--gtime CYCLES", Integer, "Global memory transfer time per burst in cycles") { |t|
      opts.options.memory_transfer_time = t
    }
    opts.on("-t", "--tdelay CYCLES", Integer, "Delay to global memory per request in cycles") { |t|
      opts.options.memory_delay = t
    }
    opts.on("-r", "--rdelay CYCLES", Integer, "Read delay to global memory per request in cycles (overrides --tdelay") { |t|
      opts.options.memory_read_delay = t
    }
    opts.on("-w", "--wdelay CYCLES", Integer, "Write delay to global memory per request in cycles (overrides --tdelay)") { |t|
      opts.options.memory_write_delay = t
    }
    opts.on("--bsize SIZE", "Transfer size (burst size) of the global memory in bytes") { |b|
      opts.options.memory_transfer_size = parse_size(b)
    }
    opts.on("--psize SIZE", "Maximum request burst size (page burst size) of the global memory in bytes") { |b|
      opts.options.memory_burst_size = parse_size(b)
    }
    
    opts.on("-d", "--dcsize SIZE", "Data cache size in bytes") { |s|
      opts.options.data_cache_size = parse_size(s)
    }
    opts.on("-D", "--dckind KIND", "Type of data cache (ideal, no, dm, lru<N>, fifo<N>)") { |p|
      opts.options.data_cache_policy = parse_policy(p)
    }
    opts.on("-s", "--scsize SIZE", "Stack cache size in bytes") { |s|
      opts.options.stack_cache_size = parse_size(s)
    }
    opts.on("-S", "--sckind KIND", %w[no ideal block ablock lblock dcache], "Type of the stack cache (no, ideal, block, ablock, lblock, dcache)") { |t|
      opts.options.stack_cache_type = t
    }
    opts.on("-m", "--icsize SIZE", "Size of the instruction/method cache in bytes") { |s|
      opts.options.instr_cache_size = parse_size(s)
    }
    opts.on("-C", "--icache KIND", %w[mcache icache], "Type of instruction cache (mcache, icache)") { |k|
      opts.options.instr_cache_kind = k
    }
    opts.on("-M", "--ickind KIND", "Policy of instruction cache (ideal, no, dm, lru<N>, fifo<N>). 'dm' is not applicable to a method cache.") { |p|
      opts.options.instr_cache_policy = parse_policy(p)
    }
    opts.on("--ibsize SIZE", "Size of an instruction cache line or method cache block in bytes") { |s|
      opts.options.instr_cache_line_size = parse_size(s)
    }

    opts.on("--set-cache-attr CACHE,NAME,VALUE", Array, "Set an attribute with a given value to the given named cache (can be specified multiple times)") { |a|
      die("Missing attribute name in --set-cache-attr #{a}") if a.length < 2
      die("Too many values for --set-cache-attr #{a}") if a.length > 3
      (opts.options.set_cache_attrs||=[]).push(a)
    }
    opts.on("--set-area-attr AREA,NAME,VALUE", Array, "Set an attribute with a given value to the given memory area (can be specified multiple times)") { |a|
      die("Missing attribute name in --set-area-attr #{a}") if a.length < 2
      die("Too many values for --set-area-attr #{a}") if a.length > 3
      (opts.options.set_area_attrs||=[]).push(a)
    }
    
    opts.on("--update-heap-syms [SIZE,NUM]", Array, "Recalculate heap-end and stack-top attribute values for the new memory size assuming NUM stacks of size SIZE (defaults to 32k,16.") { |a|
      a=[] if a.nil?
      die("Too many values for --update-heap-syms #{a}") if a.length > 2
      a[0]||="32k"
      a[1]||="16"
      opts.options.update_heap_syms = { :stack_size => parse_size(a[0]), :num_stacks => a[1].to_i }
    }

    # TODO Add options to remove attributes
    # TODO Add options to modify tool-configurations and analysis-configurations.

    opts.add_check do |options|
      die("Option --target is mandatory if no input PML is specified") unless options.triple or options.input
    end
  end
  
  def PMLConfigTool.update_memories(arch, options)
    # TODO set name of memory to configure, enable configuration of multiple memories?

    # Get or create the main memory
    main = arch.config.memories.by_name('main')
    if main.nil?
      # Create with default values
      main = PML::MemoryConfig.new("main", 0,0, 0,0,0,0)
      arch.config.memories.add(main)
    end

    # NOTE When we change the size of the memory, we might want to change the
    #      address range of the memory areas using the memory as well.. We 
    #      could let the pml.arch check function worry about that though (once
    #      it is implemented)

    # Update config
    main.kind =          options.memory_kind if options.memory_kind
    main.ramul_config =  options.ramul_config if options.ramul_config
    main.size =          options.memory_size if options.memory_size
    main.transfer_size = options.memory_transfer_size if options.memory_transfer_size

    # Should we add options to configure transfer time for reads and writes differently?
    transfer_time = options.memory_transfer_time
    read_latency  = options.memory_read_delay  || options.memory_delay
    write_latency = options.memory_write_delay || options.memory_delay

    main.read_latency        = read_latency  if read_latency
    main.read_transfer_time  = transfer_time if transfer_time
    main.write_latency       = write_latency if write_latency
    main.write_transfer_time = transfer_time if transfer_time

    main.max_burst_size = options.memory_burst_size if options.memory_burst_size
  end

  def PMLConfigTool.set_attributes(list, attrs)
    attrs.each { |name,key,value|
      entry = list.by_name(name)
      # Cache/area must exist by now for us to attach attributes
      next unless entry
      # Clean up value 
      if value.nil?
	value = true
      elsif /^\d+$/ =~ value
	value = value.to_i
      elsif /^0x[0-9a-fA-F]+$/ =~ value
	value = Integer(value)
      end
      entry.set_attribute(key, value)
    } if attrs
  end
  
  def PMLConfigTool.update_attributes(arch, options)
    set_attributes(arch.config.caches,       options.set_cache_attrs)
    set_attributes(arch.config.memory_areas, options.set_area_attrs )
  end

  def PMLConfigTool.run(pml, options)
    arch = pml.arch

    # TODO call pml.arch to make sure all required memories, caches and areas exist

    # We can handle the main memory ourselves
    update_memories(arch, options)
    
    # For caches and memory-areas, we need to ask pml.arch, this is too platform specific..
    arch.update_cache_config(options)

    # We can handle the generic cache attributes ourselves, again.
    update_attributes(arch, options)

    # Let the architecture recalculate the heap symbols
    if options.update_heap_syms
      arch.update_heap_symbols(options.update_heap_syms[:stack_size], options.update_heap_syms[:num_stacks])
    end

    # TODO call pml.arch to check and unify the machine-configuration

    # If machine-config did not exist, the PML data is out of sync now (see PMLDoc::initialize).
    pml.data['machine-configuration'] = pml.arch.config.to_pml

    pml
  end
end

if __FILE__ == $0
  synopsis=<<-EOF
    Create or modify a PML machine configuration. If no input configuration is given, a
    default configuation is generated.
  EOF
  options, args = PML::optparse([], "", synopsis) do |opts|
    opts.reads_pml
    opts.writes_pml
    PMLConfigTool.add_options(opts)
  end
  if options.input
    pml = PMLDoc.from_files(options.input)
    if options.triple and options.triple != pml.data['triple']
      die("PML triple #{pml.data['triple']} does not match option --target #{options.triple}")
    end
  else
    data = {}
    # TODO Get the default format from somewhere? a constant? read from pml.yml?
    # TODO For now, we use 'pml-0.1' to be compatible with patmos-llc, otherwise
    #      we get a value mismatch error from platin merge_streams when mixing 
    #      generated .pml files from pml-config and patmos-llc.
    data['format'] = "pml-0.1"
    data['triple'] = options.triple
    pml = PMLDoc.new(data)
  end
  outpml = PMLConfigTool.run(pml, options)
  outpml.dump_to_file(options.output, true) if options.output
end

