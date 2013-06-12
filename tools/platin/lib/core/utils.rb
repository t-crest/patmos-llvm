#
# PLATIN tool set
#
# Common utilities
#
require 'yaml'
require 'set'
require 'core/options'

$dbgs = $stderr

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
    "[#{File.basename($0).to_s.ljust(align)}] #{tag}: #{msg}"
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
