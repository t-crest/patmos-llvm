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
      @options, @checks, @help_topics = options, [], {}
      super(&block)
    end

    def register_help_topic(topic, &printer)
      assert("OptionParser: duplicate help topic") { ! @help_topics[topic] }
      @help_topics[topic] = printer
    end
    def help_topics
      @help_topics.keys
    end
    def has_help_topic(topic)
      ! @help_topics[topic].nil?
    end
    def show_help_topic(topic,io=$stderr)
      assert("OptionParser: non-existant help topic #{topic}") { @help_topics[topic] }
      @help_topics[topic].call(io)
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
    # tool can have input PML option
    def reads_pml
      self.on("-i", "--input FILE", "PML input files (can be specified multiple times)") { |f|
        (options.input||=[]).push(f)
      }
    end
    # tool writes PML file (if output is specified)
    def writes_pml
      self.on("-o", "--output FILE", "PML output file (allowed to be equivalent to an input file)") { |f| options.output = f }
    end
    # tool writes report (stdout or machinereadble)
    def writes_report
      self.on("--report [FILE]", "generate report") { |f| options.report = f || "-" }
      self.on("--append-report [KEYVALUELIST]", "append to existing report") { |kvlist|
        options.report_append = if kvlist
          Hash[kvlist.split(/,/).map { |s| s.split(/=/)}]
        else
          {}
        end
      }
    end
    # tool calculates flowfacts
    def generates_flowfacts
      self.on("--flow-fact-output NAME", "name for set of generated flow facts") { |n| options.flow_fact_output = n }
    end
    # tool generates WCET results
    def timing_output(default_name = nil) ; calculates_wcet(default_name); end
    def calculates_wcet(default_name = nil)
      self.on("--timing-output NAME", "name or prefix for set of calculated WCETs") { |n| options.timing_output = n }
      self.import_block_timing
      add_check { |options| options.timing_output = default_name unless options.timing_output } if default_name
    end
    # import WCET of basic blocks
    def import_block_timing
      self.on("--[no-]import-block-timing", "import timing and WCET frequency of basic blocks (=true)") { |b|
        options.import_block_timing = b
      }
      add_check { |options| options.import_block_timing = true if options.import_block_timing.nil? }
    end
    # tool uses call strings and allows the user to specify a custom length
    def callstring_length
      self.on("--callstring-length INTEGER", "default callstring length used in recorders (=0)") { |cl|
        options.callstring_length = cl.to_i
      }
      add_check { |options|
        options.callstring_length = 0 unless options.callstring_length
      }
    end
    # XXX: we need to think about this again
    # user should specify selection of flow facts
    def flow_fact_selection
      self.on("--flow-fact-input SOURCE,..", "flow fact sets to use (=all)") { |srcs|
        options.flow_fact_srcs = srcs.split(/\s*,\s*/)
      }
      self.on("--flow-fact-selection PROFILE,...", "flow fact filter (=all,minimal,local,rt-support-{all,local})") { |ty|
        options.flow_fact_selection = ty
      }
      self.on("--use-relation-graph", "use bitcode flowfacts via relation graph") {
        options.use_relation_graph = true
      }
      add_check { |options|
        options.flow_fact_selection = "all" unless options.flow_fact_selection
        options.flow_fact_srcs  = "all" unless options.flow_fact_srcs
      }
    end
    def accept_corrected_rgs
      self.on("--accept-corrected-rgs", "Accect corrected relation graphs for flow fact transformation") {
        options.accept_corrected_rgs = true
      }
    end
    # ELF binaries
    def binary_file(mandatory = false)
      self.on("-b", "--binary FILE", "binary file to analyze") { |f| options.binary_file = f }
      needs(:binary_file, "Option --binary is mandatory") if mandatory
    end
    # Trace entry
    def trace_entry
      self.on("--trace-entry FUNCTION", "name/label of function to trace (=main)") { |f| options.trace_entry = f }
      add_check { |options| options.trace_entry = "main" unless options.trace_entry }
    end
    # Analysis entry
    def analysis_entry(set_default = true)
      self.on("-e", "--analysis-entry FUNCTION", "name/label of function to analyse (=main)") { |f|
        options.analysis_entry = f
      }
      add_check { |options| options.analysis_entry = "main" unless options.analysis_entry or not set_default }
    end
    def stack_cache_analysis
      self.on("--use-sca-graph", "use SCA graph for stack-cache analysis") {
          options.use_sca_graph = true
      }
    end
    def target_callret_costs
      self.on("--[no-]target-call-return-costs", "Account for call and/or return miss costs for the target call. Beware, simulation and analysis can count costs differently.") { |b|
        options.target_callret_costs = b
      }
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
      opts.banner = "Usage: platin #{File.basename($0,'.rb')} OPTIONS #{arg_descr}\n\n#{synopsis}\n" 
      opts.separator("Options:")
      yield opts if block_given?
      opts.separator("")
      opts.on("--stats", "print statistics") { options.stats = true }
      opts.on("--verbose", "verbose output") { options.verbose = true }
      opts.on("--debug [TYPE]", Array, "debug output (trace,ilp,ipet,costs,wca,ait,sweet,visualize,=all)") { |d| 
        options.debug_type = d ? d.map{ |s| s.to_sym } : [:all]
      }
      opts.on_tail("-h", "--help [TOPIC]", "Show help / help on topic (#{opts.help_topics.join(", ")})") { |topic|
        if topic.nil?
          $stderr.puts opts
        elsif ! opts.has_help_topic(topic)
          $stderr.puts("Unknown help topic '#{topic}' (available: #{opts.help_topics.inspect})\n\n#{opts}")
        else
          opts.show_help_topic(topic, $stderr)
        end
        exit 0
      }
    end
    parser.parse!
    parser.check!(arg_range)
    [options, ARGV]
  end
end
