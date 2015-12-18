#
# PLATIN tool set
#
# ARM specific functionality
#

module ARM

#
# Class to (lazily) read m5 simulator trace
# yields [program_counter, cycles] pairs
#
class M5SimulatorTrace
  TIME_PER_TICK=500

  attr_reader :stats_num_items
  def initialize(elf, options)
    @elf, @options = elf, options
    @stats_num_items = 0
  end
  def each
    die("No M5 trace file specified") unless @options.trace_file
    file_open(@options.trace_file) { |fh|
      fh.each_line { |line|
        yield parse(line)
        @stats_num_items += 1
      }
    }
  end
  private
  def parse(line)
    return nil unless line
    time,event,pc,rest = line.split(/\s*:\s*/,4)
    return nil unless event =~ /system\.cpu/
    [ Integer(pc), time.to_i/TIME_PER_TICK, @stats_num_items ]
  end
end

class ExtractSymbols
  OP_CONSTPOOL=121
  OP_IMPLICIT_DEF=8
  OPCODE_NAMES={233=>/mov/}
  def ExtractSymbols.run(cmd,extractor,pml,options)
    r = IO.popen("#{cmd} -d --no-show-raw-insn '#{options.binary_file}'") do |io|
      current_label, current_ix, current_function = nil, 0, nil
      io.each_line do |line|
        if line =~ RE_FUNCTION_LABEL
          current_label, current_ix = $2, 0
          current_function = pml.machine_functions.by_label(current_label, false)
          extractor.add_symbol(current_label,Integer("0x#{$1}"))
        elsif line =~ RE_INS_LABEL
          addr, insname = $1, $2
          next unless current_function
          instruction = current_function.instructions[current_ix]
          if instruction.nil?
            if(insname[0] != "." && insname != "nop")
              warn ("No instruction found at #{current_function}+#{current_ix} instructions (#{insname})")
            end
            next
          end
          next if instruction.opcode == OP_IMPLICIT_DEF # not in disassembly
          # FIXME: We cannot reliably extract addresses of data ATM, because the disassembler
          # is not able to distinguish them. 'Data Instructions' (opcode 121) with a size
          # different from 4 will thus get incorrected addresses. We partially try to address
          # this issue by skipping data entries if the opcode is not 121
          next if(insname[0] == "." && instruction.opcode != OP_CONSTPOOL)
          extractor.add_instruction_address(current_label,current_ix, Integer("0x#{addr}"))

          # SANITY CHECK (begin)
          if (re = OPCODE_NAMES[instruction.opcode])
            if(insname !~ re)
              die ("Address extraction heuristic probably failed at #{addr}: #{insname} not #{re}")
            end
          end
          # SANITY CHECK (end)

          current_ix+=1
        end
      end
    end
    die "The objdump command '#{cmd}' exited with status #{$?.exitstatus}" unless $?.success?
  end
  RE_HEX=/[0-9A-Fa-f]/
  RE_FUNCTION_LABEL = %r{ ^
    ( #{RE_HEX}{8} ) \s # address
    <([^>]+)>:          # label
  }x
  RE_INS_LABEL = %r{ ^ \s+
    ( #{RE_HEX}+ ): \s* # address
    ( \S+ )             # instruction
    # rest
  }x

end

class Architecture < PML::Architecture
  attr_reader :config
  def initialize(triple, config)
    @triple, @config = triple, config
  end
  def Architecture.simulator_options(opts)
  end
  def config_for_clang(options)
  end
  def config_for_simulator
  end
  def simulator_trace(options, watchpoints)
    M5SimulatorTrace.new(options.binary_file, self, options)
  end
  def extract_symbols(extractor, pml, options)
    prefix="arm-#{@triple[2]}-#{@triple[3]}"
    cmd = "#{prefix}-objdump"
    ExtractSymbols.run(cmd, extractor, pml, options)
  end
end

end # module ARM

# Extend PML
module PML

# Register architecture
Architecture.register("armv4t", ARM::Architecture)
Architecture.register("armv7", ARM::Architecture)

end # module PML
