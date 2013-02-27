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
  def initialize(elf,pasim)
    @elf,@pasim = elf,pasim
    @stats_num_items = 0
  end
  def each
    die("File '#{@elf}' (ELF) not found") unless File.exist?("#{@elf}")
    begin
      IO.popen("#{@pasim} -q --debug 0 --debug-fmt trace -b #{@elf} 2>&1 1>/dev/null") do |io|
        while item=parse(io.gets) ; yield item ; @stats_num_items+=1; end
      end
    ensure
      status = $?.exitstatus
      if status == 127
        die("Running the simulator '#{@pasim}' failed: Program not found (exit status 127)")
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
  def delay_slots
    2
  end
  def simulator_trace(options)
    SimulatorTrace.new(options.binary_file, options.pasim)
  end
end

end # module patmos

# Extend PML
module PML

# Extend Option Parser
class OptionParser
  def pasim
    self.on("--pasim-command FILE", "path to pasim (=pasim)") { |f| options.pasim = f }
    self.add_check do |options|
      options.pasim = "pasim" unless options.pasim
    end
  end
end

# Register architecture
Architecture.register("patmos", PATMOS::Architecture)

end # module PML
