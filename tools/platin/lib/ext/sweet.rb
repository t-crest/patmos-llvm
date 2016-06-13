#
# PLATIN tool set
#
# Bindings to the SWEET (Swedish Execution Time) tool
#
require 'platin'
include PML
begin
  require 'rubygems'
  require 'rsec'
  include Rsec::Helpers
rescue Exception => details
  $stderr.puts "Failed to load library rsec"
  $stderr.puts "  ==> gem1.9.1 install rsec"
  $stderr.puts "Failed to load required ruby libraries"
  exit 1
end

module SWEET

  # Edge between basic blocks
  class Edge
    attr_reader :src, :target
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
    def initialize(pml)
      super()
      @pml = pml
    end
    def run(tracefile)
      @executed_instructions = 0
      lines = File.readlines(tracefile)
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


module PML

# Exception for unsupported flow facts
#
class UnsupportedFlowFactException < Exception
  attr_reader :flowfact
  def initialize(msg, ff = nil)
    super(msg)
    @flowfact = ff
  end
end

# option parser extensions
#
class OptionParser
    def alfllc_command
      self.on("--alf-llc FILE", "path to alf-llc (=alf-llc)")  { |f| options.alf_llc = f }
      self.add_check { |options|
        options.alf_llc ||= "alf-llc"
      }
    end
    def sweet_command
      self.on("--sweet-command FILE", "path to sweet (=sweet)") { |f| options.sweet = f }
      self.add_check { |options|
        options.sweet   ||= "sweet"
      }
    end
    def bitcode_file(mandatory=Proc.new { |options| false })
      self.on("--bitcode FILE", "linked bitcode file")                { |f| options.bitcode_file = f }
      self.add_check { |options|
        if mandatory.call(options)
          die_usage "Specifying a bitcode file (--bitcode) is mandatory" unless options.bitcode_file
        end
      }
    end
    def alf_file(mandatory=Proc.new { |options| false })
      self.on("--alf FILE", "ALF program model file")          { |f| options.alf_file = f }
      self.add_check { |options|
        if mandatory.call(options)
          die_usage "Specifying an ALF file is mandatory for SWEET analysis" unless options.alf_file
        end
      }
    end

    def sweet_options
      self.on("--sweet-ignore-volatiles", "treat volatile memory areas as ordinary ones") { |f|
        options.sweet_ignore_volatiles = true
      }
    end

    def sweet_flowfact_file(mandatory=Proc.new { |options| true })
      self.on("--sweet-flowfacts FILE.ff", "SWEET flowfact file") { |f| options.sweet_flowfact_file = f }
      self.add_check { |options|
        if mandatory.call(options)
          die_usage "Specifying SWEET flowfact file is mandatory" unless options.sweet_flowfact_file
        end
      }
    end
    def sweet_trace_file(mandatory = Proc.new { |options| true })
      self.on("--sweet-trace FILE.tf", "SWEET trace file") { |f| options.sweet_trace_file = f }
      self.add_check { |options|
        if mandatory && mandatory.call(options)
          die_usage "Specifying SWEET trace file is mandatory" unless options.sweet_trace_file
        end
      }
    end
  end

# class to import SWEET flow facts (format .ff) to PML
#
class SweetFlowFactImport

  def initialize(functions, fact_attributes)
    @functions = functions
    @fact_attributes = fact_attributes
  end

  def to_pml(ffsrc)
    raise UnsupportedFlowFactException.new("loop scopes not yet supported", ffsrc) if ffsrc.quantifier != :total
    raise UnsupportedFlowFactException.new("loop scopes not yet supported", ffsrc) if ffsrc.scope.stmt
    raise UnsupportedFlowFactException.new("call strings not yet supported", ffsrc) unless ffsrc.callstring.empty?

    scope = @functions.by_name(ffsrc.scope.f)
    terms = ffsrc.constraint.vector.map { |pp,factor|
      Term.new(pp_to_ref(pp), factor)
    }
    op =
      case ffsrc.constraint.op
      when "<="; "less-equal"
      when "=" ; "equal"
      else     ; raise Exception.new("Bad constraint op: #{ffsrc.constraint.op}")
      end
    flowfact = FlowFact.new(scope, TermList.new(terms), op, ffsrc.constraint.rhs,
                            @fact_attributes.dup)
    flowfact
  end

  def pp_to_ref(pp)
    if pp.kind_of?(SWEET::Edge)
      srcpp, dstpp = [pp.src,pp.target].map { |bpp| lookup_program_point(bpp) }
      raise UnsupportedFlowFactException.new("interprocedural edges not yet supported") if srcpp.function != dstpp.function
      if srcpp.kind_of?(Edge) || dstpp.kind_of?(Edge)
        raise UnsupportedFlowFactException.new("ALF edges involving LLVM edges are not supported")
      elsif dstpp.kind_of?(Instruction)
        raise UnsupportedFlowFactException.new("intrablock edges are not supported") if dstpp.ins
      elsif srcpp.kind_of?(Instruction)
        srcpp = srcpp.block
      end
      assert("supported SWEET edges should reference blocks") { srcpp.kind_of?(Block) && dstpp.kind_of?(Block) }
      Edge.new(srcpp, dstpp)
    else
      lookup_program_point(pp)
    end
  end

  def lookup_program_point(pp)
    llvm,internal = pp.split(":::")
    funname,blockname,insname,targetfunname,targetblockname = llvm.split("::")
    # For upper bounds, we could ignore the internal structure of the block
    raise UnsupportedFlowFactException.new("translation internal program points not supported") if internal
    block = @functions.by_name(funname).blocks.by_name(blockname)
    if targetblockname
      # ignore instruction for edges
      targetblock = @functions.by_name(targetfunname).blocks.by_name(targetblockname)
      Edge.new(block,targetblock)
    elsif insname
      block.instructions[insname.to_i]
    else
      block
    end
  end
end

end # end module PML

