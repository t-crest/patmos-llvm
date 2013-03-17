#
# PLATIN tool set
#
# Common utilities
#
require 'ostruct'
require 'optparse'
require 'yaml'
require 'set'

$dbgs = $stderr

module PML

  def internal_error(msg)
    raise Exception.new(format_msg("INTERNAL ERROR", msg))
  end

  def die(msg)
    $stderr.puts(format_msg("FATAL",msg))
    # $stderr.puts Thread.current.backtrace
    exit 1
  end

  def die_usage(msg)
    $stderr.puts(format_msg("USAGE","#{msg}. Try --help"))
    exit 1
  end

  def needs_options(ostruct,*opts)
    opts.each do |opt|
      internal_error("Option #{opt} not set") unless ostruct.send(opt)
    end
  end

  def warn(msg)
    $stderr.puts(format_msg("WARNING",msg))
  end

  def warn_once(msg,detail=nil)
    $warn_once ||= {}
    return if $warn_once[msg]
    detail = ": #{detail}" if detail
    warn(msg+detail.to_s)
    $warn_once[msg]=true
  end

  def info(msg)
    $stderr.puts(format_msg("INFO",msg))
  end

  def statistics(vs)
    msg = vs.map { |k,v| "#{k}: #{v}" }.join(", ")
    $stderr.puts(format_msg("STAT",msg))
  end

  def format_msg(tag,msg,align=30)
    "[#{$0.to_s.ljust(align)}] #{tag}: #{msg}"
  end

  class OptionParser < ::OptionParser
    attr_reader :options, :checks
    def initialize(options, &block)
      @options, @checks = options, []
      super(&block)
    end
    def add_check(&block)
      @checks.push(block)
    end
    def needs(option_name, msg)
      add_check { |options| die_usage(msg) unless options.send(option_name) }
    end
    # tool needs input PML option
    def needs_pml
      self.on("-i", "--input FILE", "Input PML File") { |f| options.input = f }
      needs(:input, "No input PML file specified")
    end
    # tool writes PML file (stdout or --output FILE)
    def writes_pml
      self.on("-o", "--output FILE", "Output PML File (=stdout)") { |f| options.output = f }
    end
    # tool calculates flowfacts
    def generates_flowfacts
      self.on("--flow-fact-output NAME", "Name of the flow fact generator") { |n| options.flow_fact_output = n }
    end
    # tool generates WCET results
    def calculates_wcet
      self.on("--timing-output NAME", "Name of the WCET information generator") { |n| options.timing_output = n }
    end
    # user should specify selection of flow facts
    def flow_fact_selection
      self.on("--flow-fact-input SOURCE,..", "Flow fact sources to use (=all)") { |srcs|
        options.flow_fact_srcs = srcs.split(/\s*,\s*/)
      }
      self.on("--flow-fact-selection PROFILE,...", "Flow facts set to use (=all,minimal,local,rt-support-{all,local})") { |ty|
        options.flow_fact_selection = ty
      }
      self.on("--use-relation-graph", "whether to use bitcode flowfacts via relation graph") {
        options.use_relation_graph = true
      }
      add_check { |options|
        options.flow_fact_selection = "all" unless options.flow_fact_selection
        options.flow_fact_srcs  = "all" unless options.flow_fact_srcs
      }
    end
    # ELF binaries
    def binary_file(mandatory = false)
      self.on("-b", "--binary FILE", "Name of the binary file to analyze") { |f| options.binary_file = f }
      needs(:binary_file, "Option --binary is mandatory") if mandatory
    end
    # Trace entry
    def trace_entry
      self.on("--trace-entry FUNCTION", "Name of the function to trace") { |f| options.trace_entry = f }
      add_check { |options| options.trace_entry = "main" unless options.trace_entry }
    end
    # Analysis entry
    def analysis_entry
      self.on("-e", "--analysis-entry FUNCTION", "Name of the function to analyse") { |f|
        options.analysis_entry = f
      }
      add_check { |options| options.analysis_entry = "main" unless options.analysis_entry }
    end
    # Run argument checks
    def check!(arg_range = nil)
      if arg_range.kind_of?(Array)
        die_usage "Wrong number of positional arguments" unless arg_range.length == ARGV.length
        arg_range.zip(ARGV).each { |option, arg|
          self.options.send("#{option}=".to_sym, arg)
        }
      elsif arg_range.kind_of?(Range)
        die_usage "Wrong number of positional arguments" unless arg_range.cover?(ARGV)
      end
      checks.each { |check| check.call(options) }
    end
  end

  # common option parser
  def optparse(arg_range, arg_descr, synopsis)
    options = OpenStruct.new
    parser = PML::OptionParser.new(options) do |opts|
      opts.banner = "usage: #{File.basename($0)} OPTIONS #{arg_descr}\n\n#{SYNOPSIS}\n" 
      opts.separator("Options:")
      yield opts if block_given?
      opts.separator("")
      opts.on("--stats", "print statistics") { options.stats = true }
      opts.on("--verbose", "verbose output") { options.verbose = true }
      opts.on("--debug", "debug output") { options.debug = true }
      opts.on_tail("-h", "--help", "Show this message") { $stderr.puts opts; exit 0 }
    end
    parser.parse!
    parser.check!(arg_range)
    [options, ARGV]
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
