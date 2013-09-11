#!/usr/bin/env ruby
# Prototype for patmos-clang driver replacement for WCET guided optimization

require 'ostruct'
require 'tempfile'
require 'fileutils'

def usage(err)
  $stderr.puts(err) unless err.to_s.empty?
  $stderr.puts
  $stderr.puts("Usage: patmos-clang-wcet [-c|-S|-E] <clang-option>..")
  $stderr.puts("Usage: patmos-clang-wcet -mconfig=<config.pml> [--wcet-guided-optimization] [--platin-wcet-options=<optstr>]  <clang-option>..")
  $stderr.puts("")
  $stderr.puts("This is a wrapper for patmos-clang (when producing binaries)")
  $stderr.puts("patmos-cc will configure the hardware settings for you")
  $stderr.puts("and invoke clang, platin and aiT (in a loop for optimization)")
  $stderr.puts("")
  $stderr.puts("See: patmos-clang --help")
  exit 1
end

def run(cmd)
  $stderr.puts "[patmos-clang-wcet] #{cmd}"
  system(cmd)
end

usage("") unless ARGV.length > 0

exec("patmos-clang", *ARGV) if ARGV.any? { |arg| arg == "-c" || arg == "-S" || arg == "-E" }

usage("") if ARGV.include?('--help')

options = OpenStruct.new
args = []

ARGV.each_with_index { |arg,ix|
  if arg =~ /^-mconfig=(.*)$/
    options.configfile = $1
  elsif arg =~ /--wcet-guided-optimization/
    options.guided_optimization = true
  elsif arg =~ /--platin-wcet-options=(.*)$/
    options.platin_wcet_options=$1
  elsif arg =~ /-mserialize=(.*)$/
    options.pmloutput = $1
  else
    args.push(arg)
  end
  if ix > 0 && ARGV[ix-1] == '-o'
    options.outfile = arg
  elsif arg == "-save-temps"
    options.save_temps = true
  elsif arg == '-v'
    $verbose = true # hack, but this is really prototypical for now
    options.verbose = true
  elsif arg == '--debug'
    options.debug = true
  end
}
if ! options.configfile
  usage("Option --config=<config.pml> missing.")
end
if ! options.outfile
  usage("Option -o <binary> missing.")
end
if ! options.pmloutput
  options.pmloutput = options.outfile + ".pml"
end

platin_derived_options = ""
platin_derived_options += " --outdir #{File.dirname(options.outfile).inspect}" if options.save_temps
platin_derived_options += " --debug" if options.debug
platin_derived_options += " #{options.platin_wcet_options}"
outfile =
  if options.save_temps
    Proc.new { |fname,ext|
      fname+ext
    }
  else
    Proc.new { |fname,ext|
      Tempfile.new([fname,ext]).path
    }
  end

# clang arguments
clang_argstr = args.map { |a| a.inspect }.join(" ")

# intermediate files
llvminput  = outfile.call(options.outfile,".llvm-input.pml")
llvmoutput = outfile.call(options.outfile,".llvm-output.pml")
linked_bitcode = outfile.call(options.outfile,".elf.bc")

# compile, serializing pml, elf, bc
config=`platin tool-config -t clang -i #{options.configfile} -o #{llvmoutput}`.chomp
run("patmos-clang #{config} -mpreemit-bitcode=#{linked_bitcode} #{clang_argstr}")
#run("patmos-clang #{config} -nodefaultlibs -nostartfiles -o #{options.outfile} #{linked_bitcode}")

if options.guided_optimization
  FileUtils.cp(options.outfile, options.outfile + ".preopt")

  # compute WCETs
  run("platin wcet --batch  -i #{options.configfile} -i #{llvmoutput} -b #{options.outfile} -o #{llvminput} #{platin_derived_options} --report")

  # recompile, serialize pml, elf, bc
  #run("patmos-clang #{config} -nodefaultlibs -nostartfiles -mimport-pml=#{llvminput} -o #{options.outfile} #{linked_bitcode}")
  run("patmos-clang #{config}  -mimport-pml=#{llvminput} #{clang_argstr}")
end

# compute WCETs and report
run("platin wcet --batch -i #{options.configfile} -i #{llvmoutput} -b #{options.outfile} -o #{options.pmloutput} #{platin_derived_options} --report")

unless options.save_temps
  File.unlink(llvminput)
  File.unlink(llvmoutput)
end
