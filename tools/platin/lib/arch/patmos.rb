#
# PLATIN tool set
#
# Patmos specific functionality
#

#
# pasim traces
#

module PATMOS

#
# Class to (lazily) read pasim simulator trace
# yields [program_counter, cycles] pairs
#
class SimulatorTrace
  attr_reader :stats_num_items
  def initialize(elf, options)
    @elf, @options = elf, options
    @stats_num_items = 0
  end
  def each
    die("File '#{@elf}' (ELF) not found") unless File.exist?("#{@elf}")
    if @options.trace_file
      file_open(@options.trace_file) { |fh|
        fh.each_line { |line|
          yield parse(line)
          @stats_num_items += 1
        }
      }
    else
      begin
        IO.popen("#{@options.pasim} -q --debug 0 --debug-fmt trace -b #{@elf} 2>&1 1>/dev/null") do |io|
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
    [ Integer("0x#{pc}"), Integer(cyc) ]
  end
end

class Architecture < PML::Architecture
  def initialize(triple)
    @triple = triple
  end
  def branch_delay_slots
    2
  end
  def call_delay_slots
    3
  end
  def return_delay_slots
    3
  end
  def Architecture.simulator_options(opts)
    opts.on("--pasim-command FILE", "path to pasim (=pasim)") { |f| opts.options.pasim = f }
    opts.add_check do |options|
      options.pasim = "pasim" unless options.pasim || options.trace_file
    end
  end
  def simulator_trace(options)
    SimulatorTrace.new(options.binary_file, options)
  end
end

end # module patmos

# Extend PML
module PML

# Register architecture
Architecture.register("patmos", PATMOS::Architecture)

end # module PML
