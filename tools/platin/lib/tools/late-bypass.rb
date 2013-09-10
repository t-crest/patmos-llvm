#
# platin toolset
#
# late bypass tool
#
require 'platin'
include PML

class LateBypassTool

  def LateBypassTool.add_config_options(opts)
    opts.on("-t", "--threshold [THRESHOLD]", Integer,
            "classify as unknown if a range is wider than 2^THRESHOLD") { |num|
      opts.options.range_threshold = (1 << num) unless num.nil?
    }
  end

  def LateBypassTool.add_options(opts)
    LateBypassTool.add_config_options(opts)
    opts.binary_file(true)
  end

  def LateBypassTool.has_large_range(vf, threshold)
    return vf.values.map { |v|
      r = v.range
      (r.max - r.min) >= threshold if r # else, evaluates to nil
    }.any?
  end

  def LateBypassTool.run(pml, options)
    needs_options(options, :binary_file)

    # default range threshold = 2^24
    options.range_threshold = (1 << 24) if options.range_threshold.nil?

    # select all valuefacts that come from aiT describing a load[/store]
    # possibly accessing a large address range
    valuefacts = pml.valuefacts.select { |vf|
      vf.level == "machinecode" &&
        vf.origin == "aiT" &&
        vf.programpoint.kind_of?(Instruction) &&
        # skip store instructions for now
        #['mem-address-read', 'mem-address-write'].include?(vf.variable) &&
        ['mem-address-read'].include?(vf.variable) &&
        has_large_range(vf, options.range_threshold)
    }

    # get the instruction addresses these facts refer to;
    # as they can be contained more than once (multiple contexts),
    # create a set
    addresses = valuefacts.map { |vf|
      die("Cannot obtain address for instruction "+
          "(forgot 'platin extract-symbols'?)") unless vf.programpoint.address
      vf.programpoint.address
    }.to_set


    # get the external patch_loads program and feed every address to it
    IO.popen(
      ["#{File.dirname(__FILE__)}/../ext/patch_loads", options.binary_file],
      'w') { |f|
        addresses.each { |addr| f.puts(addr) }
    }

    statistics("late-bypass",
               "number of instructions with large access range" =>
                 addresses.length) if options.stats
  end
end


if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Rewrite load from unknown memory access addresses to bypass-cache loads.
EOF
  options, args = PML::optparse([:input], "file.pml", SYNOPSIS) do |opts|
    LateBypassTool.add_options(opts)
  end
  pml = PMLDoc.from_files([options.input])
  LateBypassTool.run(pml, options)
end
