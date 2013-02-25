#
# PSK tool set
#
# Bindings to the SWEET (Swedish Execution Time) tool
#
require 'utils'
require 'trace'
include PML
begin
  require 'rubygems'
  require 'rsec'
  include Rsec::Helpers
rescue Exception => details
  $stderr.puts "Failed to load library rsec"
  $stderr.puts "  ==> gem1.9.1 install rsec"
  $stderr.put "Failed to load required ruby libraries"
  exit 1
end

module SWEET

  # Edge between basic blocks
  class Edge
    def initialize(src,target)
      @src, @target = src, target
    end
    def to_s
      "#{@src}->#{@target}"
    end
  end

  # Callstring
  class Callstring
    # ctx := [ caller, callstmt, callee ] *
    attr_reader :ctx
    def initialize(callstack)
      @ctx = callstack
    end
    def empty?
      @ctx.empty?
    end
    def to_s
      @ctx.map { |site| site.join(", ") }.join("  |  ")
    end
  end

  # Context
  class Context
    attr_reader :f,:stmt
    def initialize(func,stmt=nil)
      @f,@stmt = func,stmt
    end
    def to_s
      if @stmt
        "(#{f}, #{stmt})"
      else
        "#{f}"
      end
    end
  end

  # Vector (Linear combination of edge frequency variables)
  class Vector
    attr_reader :vec, :const
    def initialize
      @vec, @const = Hash.new(0), 0
    end
    def add(coeff,r)
      if r
        @vec[r] += coeff
      else
        @const  += coeff
      end
    end
    def Vector.negate(vec)
      vneg = {}
      vec.each do |k,v|
        vneg[k] = -v
      end
      vneg
    end
    def Vector.subtract!(v1,v2)
      v2.each do |v,coeff|
        v1[v] -= coeff
      end
      v1
    end
    def to_s
      return const.to_s if @vec.empty?
      s = @vec.map { |v,coeff|
        if coeff == 1
          v
        else
          "#{coeff} * #{v}"
        end
      }.join(" + ")
      s << " + #{const}" if(@const!=0)
      s
    end
  end

  # Constraint
  class Constraint
    # vector: (var x coeff) list
    # op: <= | =
    # rhs: int
    attr_reader :vector, :op, :rhs
    def initialize(lhs,op,rhs)
      @rhs     = rhs.const - lhs.const
      @vector  = Vector.subtract!(lhs.vec, rhs.vec)
      @op      = op
      if(op == '<')
        @op, @rhs = "<=", @rhs+1
      elsif(op == '>')
        @op, @rhs = '>=', @rhs-1
      end
      if(op == ">=")
        @op, @vector, @rhs = '<=', -@rhs, Vector.negate(@vector)
      end
    end
    def vars
      @vector.map { |var,coeff| var }
    end
    def to_s
      lhs = @vector.map { |var,coeff|
        if coeff != 1
          "#{coeff} * #{var}"
        else
          var.to_s
        end
      }.join(" + ")
      "#{lhs} #{op} #{rhs}"
    end
  end

  # Flowfact (SWEET format)
  class FlowFact
    attr_reader :type, :callstring, :scope, :quantifier, :constraint
    def initialize(type, cs,scope,quant,constraint)
      @type, @callstring, @scope, @quantifier, @constraint = type, cs, scope, quant, constraint
    end
  end


  # Parse SWEETs flow fact format using the Rsec parser
  # combinator library
  class FlowFactParser

    def flowfact(as)
      FlowFact.new(*as)
    end

    def paren(p,left='(',right=')')
      left.r >> p << right.r
    end
    def sym(c)
      symbol(c.r)
    end

    def int
      /\d+/.r.map { |v| v.to_i }
    end
    def stmt
      /[^:*+\-;,() \s\d]([^:;,() \s*+\-]|:::?)*/.r
    end
    def func
      stmt
    end
    def callstring
      caller = paren(seq_(func,stmt, skip: sym(',')))
      call   = paren(seq_(caller,func, skip: sym(',')))
      (call << /\s*/.r).star.map { |cs| Callstring.new(cs.map { |c| c.flatten }) }
    end
    def context
      paren(seq_(func,stmt,skip: sym(','))).map { |xs| Context.new(*xs) } |
        func.map { |xs| Context.new(xs) }
    end
    def quantifier
      range   = seq_(int,'..'.r,int).map { |lb,_,ub| [lb,ub] }
      foreach = paren(range.maybe,'<','>').map { |p| if(p.empty?) then :foreach else p.first end }
      total   =  ( symbol('[') << symbol(']') ).map { :total }
      quantifier = foreach | total
    end
    def build_vector
      proc do |p,*ps|
        sum = Vector.new
        sum.add(*p)
        ps.each_slice(2) do |(op, (coeff,var))|
          if(op=='-')
            sum.add(-coeff,var)
          elsif(op=='+')
            sum.add(coeff,var)
          else
            raise Exception.new("Bad addop: #{op}")
          end
        end
        sum
      end
    end
    def constraint
      expr = sym /([^<>= -]|->)+/
      count_var = seq(stmt,('->'.r >> stmt).maybe).map { |c| c[1].empty? ? c[0] : Edge.new(c[0],c[1].first) }
      cvar  = seq_( (symbol(int) << sym('*')).maybe.map { |f| f.empty? ? 1 : f.first }, count_var)
      mexpr = cvar | int.map { |p| [p,nil] }
      addop = one_of_("+-")
      expr = mexpr.join(addop).map(&build_vector)
      comparator = sym /[<>]=?|=/
      seq_(expr,comparator,expr).map { |as| Constraint.new(*as) }
    end
    def parser
      ff = seq_(callstring, context, quantifier, constraint, skip: sym(':')) << ";".r
      ff_comment = seq_(ff,sym('%%')>>/\w{4}/.r).map { |((cs,ctx,quant,constr),type)|
        FlowFact.new(type,cs,ctx,quant,constr)
      }
    end
  end
  #
  # Read sweet single-path trace and generate bitcode events
  # yields
  class TraceMonitor < ::TraceMonitor
    def initialize(tracefile, pml)
      super()
      @tracefile, @pml = tracefile, pml
    end
    def run
      @executed_instructions = 0
      lines = File.readlines(@tracefile)
      lines[1..-1].each do |l|        
        raise Exception.new("Bad trace file: more than one trace line: #{l}") if l.strip != ""
      end
      lastins = nil
      lines.first.split.each do |entry|
        break if entry == ";"
        llvm_pos, internal_pos = entry.split(':::')
        next if internal_pos # skip internal ALF statements
        fname,blockname,insindex,fsucc,blocksucc = llvm_pos.split('::')
        next if fsucc # skip edges
        function = @pml.bitcode_functions.by_name(fname)
        block = function.blocks.by_name(blockname)
        instruction = block.instructions[(insindex || 0).to_i]
        # call: start of function
        if block == function.blocks.first && ! insindex
          publish(:function, function, lastins, @executed_instructions)
        elsif block != lastins.block && instruction.name != 0
          publish(:ret, lastins, instruction, @executed_instructions)
        end
        if instruction.name == 0
          publish(:block, block, @executed_instructions)
        end
        lastins = block.instructions[insindex.to_i]
        @executed_instructions += 1
      end
      publish(:eof)
    end
  end
end # module SWEET

# extend option parser
module PML
  # option parser extensions
  class OptionParser
    def runs_llvm2alf
      self.on("--alf-llc FILE", "path to alf-llc (=alf-llc)")  { |f| options.alf_llc = f }
      self.on("--bitcode FILE", "Bitcode file")                { |f| options.bitcode_file = f }
      self.on("--alf FILE", "ALF program model file")          { |f| options.alf_file = f }
      self.add_check { |options|
        options.alf_llc ||= "alf-llc"
        die_usage "Specifying the bitcode file is mandatory for SWEET analysis" unless options.bitcode_file
        die_usage "Specifying the ALF file is mandatory for SWEET analysis"     unless options.alf_file
      }
    end
    def runs_sweet
      runs_llvm2alf
      self.on("--sweet-command FILE", "path to sweet (=sweet)") { |f| options.sweet = f }
      self.on("--sweet-ignore-volatiles", "treat volatile memory areas as ordinary ones") { |f| options.sweet_ignore_volatiles = true }
      self.add_check { |options|
        options.sweet   ||= "sweet"
      }
    end
    def sweet_flowfact_file(mandatory = true)
      self.on("--sweet-flowfacts FILE.ff", "SWEET flowfact file") { |f| options.sweet_flowfact_file = f }
      self.add_check { |options| die_usage "Specifying SWEET flowfact file is mandatory" unless options.sweet_flowfact_file } if mandatory
    end
    def sweet_trace_file(mandatory = true)
      self.on("--sweet-trace FILE.tf", "SWEET trace file") { |f| options.sweet_trace_file = f }
      self.add_check { |options| die_usage "Specifying SWEET trace file is mandatory" unless options.sweet_trace_file } if mandatory
    end
  end
# end module PML
end

# tool to invoke alf
class AlfTool
  # Internal ALF options:
  #  standalone       ... create stubs (returning \TOP) for all undefined objects
  #  ignore_volatiles ... ignore LLVM's volatile qualitifier
  #  mem_areas        ... memory areas which can be accessed using hardcoded addresses
  #  ignored_definitions ... ignore the given set of definitions
  def AlfTool.run(options, alf_opts = {})
    needs_options(options, :alf_llc, :alf_file, :bitcode_file)
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
    system(*cmd)
    die "#{options.alf_llc} failed with exit status #{$?}" unless $? == 0
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
  def AlfTool.add_options(opts)
    opts.runs_llvm2alf
  end
end

# tool to invoke sweet to generate IR-level flow facts
class SweetAnalyzeTool
  def SweetAnalyzeTool.run(pml, options)
    needs_options(options, :sweet, :alf_file, :analysis_entry, :sweet_flowfact_file)
    alfopts = {:standalone => true, :memory_areas => [(0..0xffff)], :ignored_definitions => AlfTool.default_ignored_definitions }
    alfopts[:ignore_volatiles] = true if options.sweet_ignore_volatiles
    AlfTool.run(options, alfopts)
    i_args  = [ "-i=#{options.alf_file}", "func=#{options.analysis_entry}" ]
    do_args = [ ]
    ae_args = [ "-ae", "ffg=uhss,uhsf,uhsp,unss,unsf,unsp", "vola=t", "pu" ]
    ff_args = ["-f", "co", "o=#{options.sweet_flowfact_file}" ]
    if options.sweet_ignore_volatiles
      do_args = [ "-do" , "floats=est" ]
    end
    system(options.sweet, *(i_args+do_args+ae_args+ff_args))
    die "#{options.sweet} failed with exit status #{$?}"  unless $? == 0
  end
  def SweetAnalyzeTool.add_options(opts)
    opts.analysis_entry
    opts.runs_sweet
    opts.sweet_flowfact_file
  end
end

# tool to invoke SWEET to generate IR-level traces
class SweetTraceTool
  def SweetTraceTool.run(pml, options)
    AlfTool.run(options, :standalone => true, :memory_areas => [(0..0xffff)], :ignore_volatiles => true,
                :ignored_definitions => AlfTool.default_ignored_definitions)
    system(options.sweet, "-i=#{options.alf_file}", "func=#{options.trace_entry}","-do","floats=est",
           "-ae", "vola=t", "css", "gtf=#{options.sweet_trace_file}")
    die "SWEET analysis failed"  unless $? == 0
  end

  def SweetTraceTool.add_options(opts)
    opts.analysis_entry
    opts.runs_sweet
    opts.sweet_trace_file
  end
end
