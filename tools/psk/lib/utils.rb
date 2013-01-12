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
      opts.banner = "Usage: #{File.basename($0)} OPTIONS #{arg_descr}\n#{synopsis}"
      opts.on("-i", "--input FILE", "Input PML File") { |f| options.input = f } if do_input
      opts.on("-o", "--output FILE", "Output PML File") { |f| options.output = f } if do_output
      opts.on("", "--verbose", "verbose output") { options.verbose = true }
      opts.on("", "--debug", "debug output") { options.debug = true }
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

# class providing convenient accessors and additional program information derived
# from PML files
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
  attr_reader :data, :loops
  def initialize(mf)
    @data = mf
    @hash = name.hash
    @loops = []
    @data['blocks'].each do |block|
      bref = BlockRef.new(self, block) # XXX: ugly
      if(bref.loopheader?)
        @loops.push(bref)
      end
    end
  end
  def [](k)
    @data[k]
  end
  def to_s
    "#{@data['mapsto']}/#{name}"
  end
  def ==(other)
    return false unless other.kind_of?(FunctionRef)
    name == other.name
  end
  def hash; @hash ; end
  def eql?(other); self == other ; end
  def name ; @data['name'] ; end
  def address ; @data['blocks'].first['address']; end
end

# Smart reference to a PML machine basic block
class BlockRef
  attr_reader :data,:bid,:fref
  def initialize(fref,mbb)
    @fref,@data = fref, mbb
    @bid = get_mbb_label(fref.name, @data['name'])
    @hash = @bid.hash
  end
  def loopheader?
    @data['loops'] && @data['loops'].first == @data['name']
  end
  def [](k)
    @data[k]
  end
  def to_s
    "#{fref['mapsto']}/#{bid}"
  end
  def ==(other)
    return false unless other.kind_of?(BlockRef)
    bid == other.bid
  end
  def hash; @hash ; end
  def eql?(other); self == other ; end
  def name ; self['name'] ; end
  def address ; self['address']; end
end
# Smart reference to a PML instruction
class InsRef
  attr_reader :data, :bref, :iid
  def initialize(bref,ins)
    @bref = bref
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
  def name ; @data['index']; end
  def address ; @data['address']; end
end

# Flow Fact utility class
class FlowFact
  attr_reader :data
  def initialize(initdata)
    @data = initdata.dup
    @data['lhs'] ||= []
  end
  def []=(k,v)
    @data[k] = v
  end
  def add_term(pp,factor=1)
    @data['lhs'].push({ 'factor' => factor, 'program-point' => pp })
    self
  end
  def FlowFact.block_frequency(scope, block, freq, fact_context, classification)
    flowfact = FlowFact.new(fact_context)
    flowfact['scope'] = scope
    flowfact['op'] = 'less-equal'
    flowfact['rhs'] = freq.max
    flowfact['classification'] = 'block-global'
    flowfact.add_term(FlowFact.block(block))
  end
  def FlowFact.calltargets(scope, cs, receiverset, fact_context, classification)
    flowfact = FlowFact.new(fact_context)
    flowfact['scope'] = scope
    flowfact['op'] = 'equal'
    flowfact['rhs'] = 0
    flowfact['classification'] = classification
    flowfact.add_term(FlowFact.instruction(cs), -1)
    receiverset.each do |fref| 
      flowfact.add_term(FlowFact.function(fref), 1)
    end
    flowfact
  end
  def FlowFact.loop_bound(loop, freq, fact_context, classification)
    flowfact = FlowFact.new(fact_context)
    flowfact['scope'] = FlowFact.loop(loop)
    flowfact['op'] = 'less-equal'
    flowfact['rhs'] = freq.max
    flowfact['classification'] = classification
    flowfact.add_term(FlowFact.block(loop))
    flowfact
  end
  def FlowFact.infeasible(scope, block, fact_context, classification) 
    flowfact = FlowFact.new(fact_context)
    flowfact['scope'] = scope
    flowfact['op'] = 'equal'
    flowfact['rhs'] = 0
    flowfact['classification'] = classification
    flowfact.add_term(FlowFact.block(block))
    flowfact
  end
  def FlowFact.function(f)
    { 'function' => f['name'] }
  end
  def FlowFact.block(blockref)
    { 'function' => blockref.fref.name, 'block' => blockref.name }
  end
  def FlowFact.loop(blockref)
    { 'function' => blockref.fref.name, 'loop' => blockref.name }
  end
  def FlowFact.instruction(insref)
    { 'instruction' => insref.name }.merge(FlowFact.block(insref.bref))
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
