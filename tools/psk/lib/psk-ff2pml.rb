#!/usr/bin/env ruby
#
# PSK tool set
#
# Converts SWEET .ff files to PML format
#
# TODO: support a larger variety of flow facts (call strings, loop contexts)

require 'utils'
include PML

begin
  require 'rubygems'
  require 'rsec'
  include Rsec::Helpers
rescue Exception => details
  warn "Failed to load library rsec"
  info "  ==> gem1.9.1 install rsec"
  die "Failed to load required ruby libraries"
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

end # module SWEET

class SweetAnalyzeTool
  def SweetAnalyzeTool.run(options)
    options.analysis_entry ||= "main"
    options.sweet ||= "sweet"
    options.alf_llc ||= "alf-llc"
    die_usage "Specifying the bitcode file is mandatory for SWEET analysis" unless options.bitcode_file
    die_usage "Specifying the ALF file is mandatory for SWEET analysis" unless options.sweet_alf_file
    die_usage "Specifying the SWEET/FF file is mandatory for SWEET analysis" unless options.sweet_ff_file
    system(options.alf_llc, "-march=alf", "-alf-standalone", "-alf-memory-areas=0x0000-0xffff", "-o",
           options.sweet_alf_file, options.bitcode_file)
    die "alf-llc failed" unless $? == 0
    system(options.sweet, "-i=#{options.sweet_alf_file}", "func=#{options.analysis_entry}", "-ae",
           "ffg=ub", "vola=t", "pu", "-f", "co", "o=#{options.sweet_ff_file}")
    die "SWEET analysis failed"  unless $? == 0
  end

  def SweetAnalyzeTool.add_options(opts,options)
    opts.on("-e", "--analysis-entry FUNCTION", "Name of the function to analyse") { |f| options.analysis_entry = f }
    opts.on("--alf-llc FILE", "path to alf-llc (=alf-llc)") { |f| options.alf_llc = f }
    opts.on("--sweet-command FILE", "path to sweet (=sweet)") { |f| options.sweet = f }
    opts.on("--bitcode FILE", "Bitcode file")          { |f| options.bitcode_file = f }
    opts.on("--sweet-alf FILE", "SWEET flowfact file") { |f| options.sweet_alf_file = f }
    opts.on("--sweet-ff  FILE", "SWEET flowfact file") { |f| options.sweet_ff_file = f }
  end
end

class UnsupportedFlowFactException < Exception
  def initialize(msg)
    super(msg)
  end
end

class SweetFlowFactImport
  def initialize(functions, fact_context)
    @functions = functions
    @fact_context = fact_context
  end
  def to_pml(ffsrc)
    raise UnsupportedFlowFactException.new("loop scopes not yet supported") if ffsrc.quantifier != :total
    raise UnsupportedFlowFactException.new("loop scopes not yet supported") if ffsrc.scope.stmt
    raise UnsupportedFlowFactException.new("call strings not yet supported") unless ffsrc.callstring.empty?
    scope = @functions.by_name(ffsrc.scope.f)
    terms = ffsrc.constraint.vector.map { |pp,factor|
      Term.new(pp_to_pml(pp).ref, factor)
    }
    op =
      case ffsrc.constraint.op
      when "<="; "less-equal"
      when "=" ; "equal"
      else     ; raise Exception.new("Bad constraint op: #{ffsrc.constraint.op}")
      end
    flowfact = FlowFact.new(scope.ref, TermList.new(terms), op, ffsrc.constraint.rhs)
    flowfact.add_attributes(@fact_context)
    flowfact
  end
  def pp_to_pml(pp)
    raise UnsupportedFlowFactException.new("edge program points not yet supported") if pp.kind_of?(SWEET::Edge)
    llvm,internal = pp.split(":::")
    fun,block,ins = llvm.split("::")
    # For upper bounds, we could ignore the internal structure of the block
    raise UnsupportedFlowFactException.new("translation internal program points not supported") if internal
    raise UnsupportedFlowFactException.new("instruction program points not supported") if ins
    @functions.by_name(fun).blocks.by_name(block)
  end
end

class SweetImportTool
  def SweetImportTool.run(ff_file,pml,options)
    parser = SWEET::FlowFactParser.new.parser
    converter = SweetFlowFactImport.new(pml.bitcode_functions, 'level' => 'bitcode', 'origin' => 'SWEET')
    ffs = []
    added, skipped, reasons, set = 0,0, Hash.new(0), {}
    File.readlines(ff_file).map do |s|
      begin
        ff = parser.parse!(s)        
        ff_pml = converter.to_pml(ff)
        if set[ff_pml]
          reasons["duplicate"] += 1
          skipped+=1
        else
          set[ff_pml] = true
          pml.flowfacts.add(ff_pml)
          added += 1
        end
      rescue UnsupportedFlowFactException=>detail
        reasons[detail.to_s] += 1
        skipped += 1
      end
    end
    if options.verbose
      $dbgs.puts "Parsed #{skipped+added} flow facts, added #{added}"
      $dbgs.puts "Reasons for skipping flow facts: "
      reasons.each do |k,count|
        $dbgs.puts "  #{k} (#{count})"
      end
    end
    pml
  end
  def SweetImportTool.add_options(opts,options)
  end
end


if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Translate SWEET flow facts (format FF) to pml flow facts.
EOF
  options, args = PML::optparse(1, "file.ff", SYNOPSIS, :type => :io) do |opts,options|
    SweetImportTool.add_options(opts,options)
  end
  SweetImportTool.run(args.first, PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
