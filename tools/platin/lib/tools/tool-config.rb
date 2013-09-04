#
# platin toolchain: pkg-config
#
# small tool to configure clang and pasim with the right machine configuration
#
require 'platin'
include PML

class ToolConfigTool
  def ToolConfigTool.add_options(opts)
    opts.on("-t","--tool TOOL", "tool to configure (=clang)") { |t| opts.options.tool = t }
    opts.on("-m","--mode MODE", "configuration mode (tool specific)") { |m| opts.options.mode = m }
    opts.add_check do |options|
      die("Option --tool is mandatory") unless options.tool
    end
  end
  def ToolConfigTool.run(pml, options)
    needs_options(options, :tool)
    case options.tool
    when 'clang'
      opts = pml.arch.config_for_clang
      roots = pml.analysis_configurations.map { |cs| cs.program_entry }.inject(Set.new) { |s,v| s.add(v) if v; s }.to_a
      roots = ['main'] if roots.empty?
      opts.push("-mserialize=#{options.output.to_s.inspect}")
      opts.push("-mserialize-roots=#{roots.join(",").inspect}")
    when 'simulator'
      opts = pml.arch.config_for_simulator
    else
      die("platin tool configuration: Unknown tool specified: #{options.tool} (clang,simulator)")
    end
    puts opts.join(" ")
  end
end

if __FILE__ == $0
  synopsis=<<-EOF
    Configure external tools to use the correct hardware (timing) model.
    Similar to pkg-config, this tool writes the arguments to be passed to $stdout. It uses
    architecture specific implementations, and is typically used like this:

      patmos-clang $(platin pkg-config -t clang -i config.pml -o llvm.pml) -o myprogram myprogram.c

    Supported Tools: clang, simulator
  EOF
  options, args = PML::optparse([], "", synopsis) do |opts|
    opts.needs_pml
    opts.writes_pml
    ToolConfigTool.add_options(opts)
  end
  ToolConfigTool.run(PMLDoc.from_files(options.input), options)
end

