#
# PLATIN tool set
#
# Option handling facilities
#
require 'ostruct'
require 'optparse'

module PML
  def needs_options(ostruct,*opts)
    opts.each do |opt|
      internal_error("Option #{opt} not set") unless ostruct.send(opt)
    end
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
      self.on("-i", "--input FILE", "PML input files (can be specified multiple times)") { |f|
        (options.input||=[]).push(f)
      }
      needs(:input, "No input PML file specified")
    end
    # tool writes PML file (if output is specified)
    def writes_pml
      self.on("-o", "--output FILE", "PML output file (allowed to be equivalent to an input file)") { |f| options.output = f }
    end
    # tool writes report (stdout or machinereadble)
    def writes_report
      self.on("--report [FILE]", "Generate report") { |f| options.report = f || "-" }
    end
    # tool calculates flowfacts
    def generates_flowfacts
      self.on("--flow-fact-output NAME", "Name of the flow fact generator") { |n| options.flow_fact_output = n }
    end
    # tool generates WCET results
    def calculates_wcet(default_name = nil)
      self.on("--timing-output NAME", "Name of the WCET information generator") { |n| options.timing_output = n }
      add_check { |options| options.timing_output = default_name unless options.timing_output } if default_name
    end
    # tool uses call strings and allows the user to specify a custom length
    def callstring_length
      self.on("--callstring-length INTEGER", "default callstring length used in recorders (=0)") { |recorder_cl|
        options.callstring_length = recorder_cl.to_i
      }
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
