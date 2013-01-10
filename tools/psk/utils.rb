#
# Common utilities for the psk ruby implementations
#
require 'ostruct'
require 'optparse'
require 'yaml'
require 'set'

module PMLUtils
  RE_HEX=/[0-9A-Fa-f]/
  def die(msg)
    $stderr.puts msg
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

  def optparse(arg_range, arg_descr, synopsis, opts)
    options = OpenStruct.new
    do_input  = opts[:type] == :io || opts[:type] == :input
    do_output = opts[:type] == :io || opts[:type] == :output
    parser = OptionParser.new do |opts|
      opts.banner = "Usage: #{$0} OPTIONS #{arg_descr}\n#{synopsis}"
      opts.on("-i", "--input FILE", "Input PML File") { |f| options.input = f } if do_input
      opts.on("-o", "--output FILE", "Output PML File") { |f| options.output = f } if do_output
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
    die "Wrong number of positional arguments. Try --help" unless arg_range_ok
    if do_input
      die "Option --input is mandatory. Try --help" unless options.input
      [options, ARGV, PML.from_file(options.input) ]
    else
      [options, ARGV]
    end
  end

  def dquote(str)
    '"' + str + '"'
  end

  # TODO create label depending on architecture?
  def get_mbb_label(func, block)
    func  = func['name']  if func.kind_of?(Hash)
    block = block['name'] if block.kind_of?(Hash)
    ".LBB#{func}_#{block}"
  end
  def parse_mbb_label(label)
    label =~ /\A\.LBB(\d+)_(\d+)$/
    [$1.to_i, $2.to_i] if $1
  end

  def merge_ranges(r1,r2=nil)
    die "merge_ranges: first argument is nil" unless r1
    r1=Range.new(r1,r1) unless r1.kind_of?(Range)
    return r1 unless r2
    [r1.min,r2.min].min .. [r1.max,r2.max].max
  end
end

# class providing convenient additional program information derived from PML
# files
class PML
  attr_reader :data, :functions
  def initialize(data_or_io)
    stream = if data_or_io.kind_of?(Array)
               data_or_io
             elsif data_or_io.kind_of?(IO)
               stream = YAML::load_stream(data_or_io)
               stream = stream.documents if stream.respond_to?(:documents) # ruby 1.8 compat
               stream
             elsif
               [data_or_io]
             end
    if stream.length == 1
      @data = stream[0]
    else
      @data = PML.merge_stream(stream)
    end
    build_information
  end
  def [](key)
    @data[key]
  end
  def to_s
    @data.to_s
  end
  def dump_to_file(filename)
    if filename.nil?
      dump($>)
    else
      File.open(filename, "w") do |fh|
        dump(fh)
      end
    end
  end
  def dump(io)
    io.write(YAML::dump(data))
  end
  def bf(name)
    @functions['src'][name]
  end
  def mf(name)
    @functions['dst'][name]
  end
  def build_information
    @functions = { 'src' => {}, 'dst' => {} }
    @dstfunmap = {}
    @data['bitcode-functions'].each { |f| @functions['src'][f['name']] = f }
    @data['machine-functions'].each { |f|
      die "Duplicate machine function #{f['name']}" if @functions['dst'][f['name']]
      @functions['dst'][f['name']] = f
      @dstfunmap[f['mapsto']] = f
    }
  end
  def mf_mapping_to(bitcode_name)
    @dstfunmap[bitcode_name]
  end
  def PML.from_file(filename)
    File.open(filename) { |fh| PML.new(fh) }
  end
  def PML.merge_stream(stream)
    merged_doc = {}
    stream.each do |doc|
      doc.each do |k,v|
        if(v.kind_of? Array)
          (merged_doc[k]||=[]).concat(v)
        elsif(! merged_doc[k])
          merged_doc[k] = doc[k]
        elsif(merged_doc[k] != doc[k])
          raise Exception.new("psk-merge: mismatch in non-list attribute #{k}: #{merged_doc[k]} and #{doc[k]}")
        end
      end
    end
    merged_doc
  end
end

# Smart Reference to a PML function
class FunctionRef
  def initialize(mf)
    @mf = mf
    @hash = name.hash
  end
  def [](k)
    @mf[k]
  end
  def to_s
    "#{@mf['mapsto']}/#{name}"
  end
  def ==(other)
    return false unless other.kind_of?(FunctionRef)
    name == other.name
  end
  def name ; @mf['name'] ; end
  def hash; @hash ; end
  def eql?(other); self == other ; end
end

# Smart reference to a PML machine basic block
class BlockRef
  attr_reader :bid,:fref
  def initialize(mf,mbb)
    @fref,@mbb = FunctionRef.new(mf), mbb
    @bid = get_mbb_label(mf,mbb)
    @hash = @bid.hash
  end
  def [](k)
    @mbb[k]
  end
  def to_s
    "#{fref['mapsto']}/#{bid}"
  end
  def ==(other)
    return false unless other.kind_of?(BlockRef)
    bid == other.bid
  end
  def name ; self['name'] ; end
  def hash; @hash ; end
  def eql?(other); self == other ; end
end
# Smart reference to a PML instruction
class InsRef
  attr_reader :data, :bref, :iid
  def initialize(mf,mbb,ins)
    @bref = BlockRef.new(mf,mbb)
    @data = ins
    @iid = "#{@bref.bid}_#{ins['index']}"
    @hash = @iid.hash
  end
  def fref
    bref.fref
  end
  def [](k)
    @data[k]
  end
  def to_s
    "#{fref['mapsto']}/#{iid}"
  end
  def ==(other)
    return false unless other.kind_of?(BlockRef)
    iid == other.iid
  end
  def hash; @hash ; end
  def eql?(other); self == other ; end
end

class FrequencyRecord
  attr_reader :cycles, :freqs, :calltargets
  def initialize
    @calltargets = {}
  end
  def start(cycles)
    @cycles_start = cycles
    @current_record = Hash.new(0)
  end
  def increment(bb)
    @current_record[bb] += 1 if @current_record
  end
  def call(callsite,callee)
    (@calltargets[callsite]||=Set.new).add(callee) if @current_record && callsite
  end
  def stop(cycles)
    die "Recorder: stop without start" unless @current_record
    @cycles = merge_ranges(cycles - @cycles_start, @cycles)
    unless @freqs
      @freqs = {}
      @current_record.each do |bref,count|
        @freqs[bref] = count .. count
      end
    else
      @current_record.each do |bref,count|
        if ! @freqs.include?(bref)
          @freqs[bref] = 0 .. count
        else
          @freqs[bref] = merge_ranges(count, @freqs[bref])
        end
      end
      @freqs.each do |bref,count|
        @freqs[bref] = merge_ranges(count, 0..0) unless @current_record.include?(bref)
      end
    end
  end
  def dump(io=$>)
    (io.puts "No records";return) unless @freqs
    io.puts "Cycles: #{cycles}"
    io.puts "---"
    @freqs.each do |bref,freq|
      io.puts "#{bref.to_s.ljust(15)} \\in #{freq}"
    end
    io.puts "---"
    @calltargets.each do |site,recv|
      io.puts "#{site} calls #{recv.to_a.join(", ")}"
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
