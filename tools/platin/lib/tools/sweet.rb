#
# PLATIN tool set
#
# SWEET bridge
#
require 'platin'
require 'ext/sweet'
include PML

class AlfTool
  def AlfTool.add_config_options(opts)
    opts.alfllc_command
  end
  def AlfTool.add_options(opts)
    AlfTool.add_config_options(opts)
    opts.bitcode_file
    opts.alf_file
  end
  # Internal ALF options:
  #  standalone       ... create stubs (returning \TOP) for all undefined objects
  #  ignore_volatiles ... ignore LLVM's volatile qualitifier
  #  mem_areas        ... memory areas which can be accessed using hardcoded addresses
  #  ignored_definitions ... ignore the given set of definitions
  def AlfTool.run(options, alf_opts = {})
    needs_options(options, :alf_llc, :alf_file, :bitcode_file)
    raise MissingToolException.new("alf-llc not found") unless which(options.alf_llc)
    cmd = [ options.alf_llc, "-march=alf", "-o", options.alf_file ]
    cmd.push("-alf-standalone") if alf_opts[:standalone]
    cmd.push("-alf-ignore-volatiles") if alf_opts[:ignore_volatiles]
    cmd.push("-alf-ignore-definitions=#{alf_opts[:ignored_definitions].join(",")}") if alf_opts[:ignored_definitions]
    if alf_opts[:memory_areas]
      areas = alf_opts[:memory_areas].map { |mem_area|
        sprintf("0x%x-0x%x",mem_area.min, mem_area.max)
      }.join(",")
      cmd.push("-alf-memory-areas=#{areas}")
    end
    cmd.push(options.bitcode_file)
    $stderr.puts("Executing #{cmd.join(" ")}") if options.verbose
    unless safe_system(*cmd)
      die "#{options.alf_llc} failed with exit status #{$?}"
    end
  end
  def AlfTool.default_ignored_definitions
    %w{__adddf3 __addsf3 __divdf3 __divsf3 __eqdf2} +
      %w{__eqsf2 __extendsfdf2 __fixdfdi __fixdfsi __fixsfdi} +
      %w{__fixsfsi __fixunsdfdi __fixunsdfsi __fixunssfdi __fixunssfsi} +
      %w{__floatdidf __floatdisf __floatsidf __floatsisf __floatundidf} +
      %w{__floatundisf __floatunsidf __floatunsisf __gedf2 __gesf2} +
      %w{__gtdf2 __gtsf2 __ledf2 __lesf2 __ltdf2} +
      %w{__ltsf2 __malloc_sbrk_base __muldf3 __mulsf3 __nedf2} +
      %w{__nesf2 __sigtramp __sigtramp_r __subdf3 __subsf3} +
      %w{__truncdfsf2 __unorddf2 __unordsf2 _exit _malloc_r} +
      %w{_malloc_trim_r _raise_r _sbrk _sbrk.heap_ptr _sbrk_r} +
      %w{_signal_r _start abort exit malloc memcpy} +
      %w{memmove memset raise setjmp longjmp signal}
  end
end

# tool to invoke sweet to generate IR-level flow facts
class SweetAnalyzeTool
  def SweetAnalyzeTool.add_config_options(opts)
    opts.alfllc_command
    opts.sweet_command
  end
  def SweetAnalyzeTool.add_options(opts)
    SweetAnalyzeTool.add_config_options(opts)
    opts.analysis_entry
    opts.bitcode_file
    opts.alf_file
    opts.sweet_options
    opts.on("--sweet-generate-trace") { opts.options.sweet_generate_trace = true }
    opts.sweet_flowfact_file
    opts.sweet_trace_file(false)
    opts.add_check { |options|
      if options.sweet_generate_trace
        options.sweet_ignore_volatiles = true
        die("Option sweet_trace_file is mandatory given --sweet-generate-trace") unless options.sweet_trace_file
      end
    }
  end
  def SweetAnalyzeTool.run(pml, options)
    needs_options(options, :sweet, :alf_file, :analysis_entry, :sweet_flowfact_file)
    alfopts = {:standalone => true, :memory_areas => [(0..0xffff)], :ignored_definitions => AlfTool.default_ignored_definitions }
    alfopts[:ignore_volatiles] = true if options.sweet_ignore_volatiles
    AlfTool.run(options, alfopts)
    i_args  = [ "-i=#{options.alf_file}", "func=#{options.analysis_entry}" ]
    do_args = [ ]
    if options.sweet_ignore_volatiles
      do_args.push("floats=est")
    end
    ae_args = [ "-ae", "ffg=uhss,uhsf,uhsp,unss,unsf,unsp,uesp,uesf,uess,ubns", "vola=t" ]
    if f = options.sweet_generate_trace
      ae_args.push('css')
      ae_args.push("gtf=#{options.sweet_trace_file}")
    else
      ae_args.push('pu')
    end
    ff_args = ["-f", "co", "o=#{options.sweet_flowfact_file}" ]
    if options.sweet_ignore_volatiles
      do_args = [ "-do" , "floats=est" ]
    end
    raise MissingToolException.new("sweet not found") unless which(options.sweet)
    cmd = ([options.sweet] + i_args + do_args + ae_args + ff_args)
    version, commands, parsed = nil, [], []
    $stderr.puts("Executing #{cmd.join(" ")}") if options.verbose
    IO.popen(cmd + [:err => [:child,:out]]) { |sweet_io|
      while l = sweet_io.gets
        if l =~ /SWEET version (.*)/
          version = $1
        elsif l =~ /Parsing ALF file (.*)/
          parsed.push($1)
        elsif l =~ /Executing command (.*)/
          commands.push($1)
        else
          warn("SWEET reports: #{l}")
        end
      end
    }
    unless $? == 0
      die "#{options.sweet} failed with exit status #{$?}"
    end
    info("Successfully ran SWEET version #{version}") if options.verbose
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Run the Swedish Execution Time Analysis tool (SWEET)
EOF
  options, args = PML::optparse([], "", SYNOPSIS) do |opts|
    opts.needs_pml
    SweetAnalyzeTool.add_options(opts)
  end
  SweetAnalyzeTool.run(PMLDoc.from_files(options.input), options)
end
