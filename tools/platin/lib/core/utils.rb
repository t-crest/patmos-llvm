#
# PLATIN tool set
#
# Common utilities
#
require 'yaml'
require 'set'
require 'core/options'

module PML
  def file_open(path,mode="r")
    internal_error "file_open: nil" unless path
    if(path=="-")
      case mode
      when "r" ; yield $stdin
      when "w" ; yield $stdout
      when "a" ; yield $stdout
      else ; die "Cannot open stdout in mode #{mode}"
      end
    else
      File.open(path,mode) { |fh|
        yield fh
      }
    end
  end

  def safe_system(*args)
    # make sure spawned process get killed at exit
    # hangs if subprocess refuses to terminate
    pids = []  # holds the spawned pids
    trap(0) do # kill spawned pid(s) when terminating
      pids.each do |pid|
        next unless pid
        begin
          Process.kill("TERM",pid)
          $stderr.puts("Terminated spawned child with PID #{pid}")
        rescue SystemCallError
          # killed in the meantime
        end
      end
    end
    begin
      pids.push(spawn(*args))
    rescue SystemCallError
      return nil
    end
    Process.wait(pids.first)
    trap(0, "DEFAULT")
    $? == 0
  end

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

  #
  # output debug message(s)
  #  type               ... the debug type for the message(s) (e.g., 'ipet')
  #  options.debug_type ... debug types to print; either :all or a specific debug type
  #  block              ... either returns one message, or yields several ones
  # Usage 1:
  #  debug(@options,'ipet') { "number of constraint: #{constraints.length}" }
  # Usage 2:
  #  debug(@options,'ipet') { |&msgs| constraints.each { |c| msgs.call("Constraint: #{c}") } }
  #
  def debug(options, type, &block)
    return unless options.debug_type == :all || options.debug_type == type
    msgs = []
    r = block.call { |m| msgs.push(m) }
    msgs.push(r) if msgs.empty?
    msgs.compact.each { |msg|
      $stderr.puts(format_msg("DEBUG",msg))
    }
  end

  class DebugIO
    def initialize(io=$stderr)
      @io = io
    end
    def puts(str)
      @io.puts(format_msg("DEBUG",str))
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

  def statistics(mod,vs,align=47)
    vs.each { |k,v|
      key = "#{mod}: #{k}".ljust(align)
      msg = "#{key} #{v}"
      $stderr.puts(format_msg("STAT",msg))
    }
  end

  def format_msg(tag,msg,align=-1)
    "[platin] #{tag}: #{msg}"
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
