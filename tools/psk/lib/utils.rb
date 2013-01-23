#
# Common utilities for the psk ruby implementations
#
require 'ostruct'
require 'optparse'
require 'yaml'
require 'set'
require 'pml'

$dbgs = $stderr

module PML
  def internal_error(msg)
    raise Exception.new("[#{$0}] INTERNAL ERROR: #{msg}")
  end
  def die(msg)
    $stderr.puts "[#{$0}] FATAL: #{msg}"
    puts Thread.current.backtrace
    exit 1
  end
  def die_usage(msg)
    $stderr.puts "#{$0}: #{msg}. Try --help."
    exit 1
  end
  def warn(msg)
    $stderr.puts "[#{$0}] WARNING #{msg}"
  end
  def warn_once(msg,detail=nil)
    $warn_once ||= {}
    return if $warn_once[msg]
    detail = ": #{detail}" if detail
    warn(msg+detail.to_s)
    $warn_once[msg]=true
  end
  def info(msg)
    $stderr.puts "[#{$0}] INFO #{msg}"
  end
  def optparse(arg_range, arg_descr, synopsis, opts)
    options = OpenStruct.new
    do_input  = opts[:type] == :io || opts[:type] == :input
    do_output = opts[:type] == :io || opts[:type] == :output
    parser = OptionParser.new do |opts|
      opts.banner = "usage: #{File.basename($0)} OPTIONS #{arg_descr}\n\n#{SYNOPSIS}\n" 
      opts.on("-i", "--input FILE", "Input PML File") { |f| options.input = f } if do_input
      opts.on("-o", "--output FILE", "Output PML File") { |f| options.output = f } if do_output
      opts.on("--verbose", "verbose output") { options.verbose = true }
      opts.on("--debug", "debug output") { options.debug = true }
      yield [opts,options] if block_given?
      opts.on_tail("-h", "--help", "Show this message") { $stderr.puts opts; exit 0 }
    end.parse!
    arg_range_ok = if ! arg_range
                     true
                   elsif arg_range.kind_of?(Integer)
                     ARGV.length == arg_range
                   else
                     arg_range.cover?(ARGV.length)
                   end
    die_usage "Wrong number of positional arguments" unless arg_range_ok
    if do_input
      die_usage "Option --input is mandatory" unless options.input
      [options, ARGV]
    else
      [options, ARGV]
    end
  end
end

# 1.8 compat
if RUBY_VERSION =~ /^1\.8\.?/
  class Range
    def cover?(v)
      v >= min && v <= max
    end
  end
end

# Development helpers
class Hash
  def dump(io=$>)
    self.each do |k,v|
      puts "#{k.ljust(24)} #{v}"
    end
  end
end
