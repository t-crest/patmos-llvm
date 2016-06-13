#
# platin toolkit
#
# The pml tool is used to
# - validate PML files
# - merge PML files into a single file
# - print statistics for a PML file
# - visualize PML files
#
require 'set'
require 'platin'
include PML

require 'tools/transform'

class InspectTool
  attr_reader :pml, :options
  def initialize(pml, options)
    @pml, @options = pml, options
  end

  # TODO same code as in wcet tool, move to library?
  def time(descr)
    begin
      t1 = Time.now
      yield
      t2 = Time.now
      info("Finished #{descr.ljust(35)} in #{((t2-t1)*1000).to_i} ms")
    end
  end

  # TODO same code as in wcet tool, move to library?
  def transform_down(srcs, output)
    time("Flow Fact Transformation #{srcs}") do
      opts = options.dup
      opts.flow_fact_selection ||= "all"
      opts.flow_fact_srcs = srcs
      opts.flow_fact_output = output
      opts.transform_action = 'down'
      TransformTool.run(pml, opts)
    end
  end

  def print_loops(functions, level, ffmap, unbounded, io)
    functions.each do |fun|
      fun.loops.each do |loop|
        next if unbounded and ffmap[loop.loopheader]
        io.puts "=== #{level} loop #{loop.to_s} ==="
        (ffmap[loop.loopheader]||[]).each { |ff|
          io.puts "bounded by #{ff.to_s}"
        }
      end
    end
  end

  def show_loops(unbounded, io = $stdout)
    ffmap = {}
    @pml.flowfacts.each do |ff|
      s, b, rhs = ff.get_block_frequency_bound
      next if b.nil?
      (ffmap[b.programpoint.block]||=[]).push(ff)
    end

    print_loops(@pml.bitcode_functions, "bitcode", ffmap, unbounded, io)
    print_loops(@pml.machine_functions, "machinecode", ffmap, unbounded, io)
  end

  def InspectTool.run(pml,options)
    tool = InspectTool.new(pml, options)

    tool.transform_down(["llvm.bc"],"llvm")
    tool.transform_down(["user.bc"],"user")

    tool.show_loops(options.show_unbounded_loops)
  end
  def InspectTool.add_options(opts)
    TransformTool.add_options(opts)
    #opts.on("--find-loop-bounds", "find all loops and print their loop bounds") { opts.options.show_loop_bounds = true }
    opts.on("--find-unbounded-loops", "find all loops that have no loop bounds") { opts.options.show_unbounded_loops = true }
  end
end

if __FILE__ == $0
  synopsis="Inspect the program structure and flow/value facts"
  options, args = PML::optparse([], "", synopsis) do |opts|
    opts.needs_pml
    InspectTool.add_options(opts)
  end
  InspectTool.run(PMLDoc.from_files(options.input), options)
end
