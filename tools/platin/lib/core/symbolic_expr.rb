#
# platin toolkit
#
# symbolic expressions
#
require 'set'
begin
  require 'rubygems'
  require 'rsec'
  include Rsec::Helpers
rescue Exception => details
  $stderr.puts "Failed to load library rsec"
  $stderr.puts "  ==> gem install rsec"
  $stderr.puts "Failed to load required ruby libraries"
  exit 1
end

module PML

class NoLoopBoundAvailableException < Exception
  attr_reader :loop
  def initialize(msg, loop)
    super(msg + ": #{loop}")
    @loop = loop
  end
end

# Symbolic Expressions, for example loop bounds depending on an argument
# The syntax used is that of LLVM, as we import their Scalar Evolution
# expressions
class SymbolicExpression

  # Parse symbolic expression (LLVM 3.4's Scalar Evolution Expression format)
  def SymbolicExpression.parse(str)
    SymbolicExpressionParser.parse(str)
  end

  # Convert constant expression to integer. Fails if expression is not #constant?
  def to_i
    raise Exception.new("SymbolicExpression#to_i: not constant")
  end

  # Evaluate the symbolic expression, given the variable environment env
  # and the loop environment lenv. The latter is optional, and only needed
  # if chains of recurrences are present.
  #
  # +env+::  a map from variable names (+String+) to values (+Integer+)
  # +lenv+:: a loop environent
  #
  def eval(env, lenv = nil)
    raise Exception.new("SEAFfineRec#eval: no loop environment given") unless lenv
    resolve_loops(lenv).eval(env,lenv)
  end

  # map all symbolic names in the symbolic expression
  # +map_names+ yields a pair +[type, name]+ to the caller
  #
  #  - +[:variable, v]+ ... Variable name
  #  - +[:loop, l]+     ... Loop block used in a CHR
  #
  def map_names
    raise Exception.new("map_names not implemented for #{self.class}")
  end

  def constant;            nil  ; end
  def constant?;           false; end
  def chainOfRecurrences?; false; end

  def +(o) ;    SEBinary.collect_fold('+',self,o) ; end
  def add(*os); SEBinary.collect_fold('+',self,*os) ; end
  def -(o) ;    self + (-o) ; end
  def *(o) ;    SEBinary.collect_fold('*',self,o) ; end
  def -@   ;    SEBinary.create('*',self,SEInt.new(-1)) ; end
  def smax(o);  SEBinary.create('smax',self,o) ; end
  def umax(o);  SEBinary.create('umax',self,o) ; end
  def smin(o);  SEBinary.create('smin',self,o) ; end
  def umin(o);  SEBinary.create('umin',self,o) ; end
  def sdiv(o);  SEBinary.create('/s',self,o) ; end
  def ==(other)
    return false if other.nil?
    unless other.kind_of?(SymbolicExpression)
      raise Exception.new("unexpected comparsion with symbolic expression")
    end
    return super(other)
  end
  def eql?(other); self == other ; end
end

class SEInt < SymbolicExpression
  def initialize(int)
    raise Exception.new("Bad integer: #{int}/#{int.class}") unless int.kind_of?(Integer)
    @num = int
  end
  def constant; to_i;  end
  def constant?; true; end
  def to_i; @num     ; end
  def to_s; @num.to_s; end
  def eval(_,lenv=nil);     @num  ; end
  def resolve_loops(lenv) ; self ; end
  def referenced_loops ; Set.new; end
  def referenced_vars  ; Set.new; end
  def map_names(&block) ; self ; end
end

class SEVar < SymbolicExpression
  attr_reader :var
  def initialize(str)
    @var = str
  end

  def eval(env, lenv)
    if v = env[@var]
      v
    else
      raise Exception.new("SymbolicExpression#eval: unknown variable #{@var}")
    end
  end

  def referenced_vars
    Set[@var]
  end

  def map_names(&block)
    new_name = block.call(:variable,@var)
    SEVar.new(new_name)
  end

  def referenced_loops
    Set.new
  end

  def resolve_loops(lenv)
    self
  end

  def to_s
    @var.to_s
  end
end

class SEBinary < SymbolicExpression
  attr_reader :op, :a, :b
  def initialize(op, a, b)
    @op, @a, @b = op, a, b
  end

  def SEBinary.create(op, a, b)
    a = SEInt.new(a) if a.kind_of?(Integer)
    b = SEInt.new(b) if b.kind_of?(Integer)
    expr = SEBinary.new(op, a, b)
    if a.constant? && b.constant?
      SEInt.new(expr.eval({}))
    elsif lunit(op) && a.constant == lunit(op)
      b
    elsif runit(op) && b.constant == runit(op)
      a
    elsif zero(op) && (a.constant == zero(op) || b.constant == zero(op))
      SEInt.new(zero(op))
    else
      expr
    end
  end

  def SEBinary.unit(op)
    case op
    when '+' ; then 0
    when '*' ; then 1
    when 'umax' ; then 0
    else ; nil
    end
  end

  def SEBinary.lunit(op)
    SEBinary.unit(op)
  end

  def SEBinary.runit(op)
    case op
    when '/s' ; then 1
    when '/u' ; then 1
    else ; SEBinary.unit(op)
    end
  end

  def SEBinary.zero(op)
    if op == '*' then 0 else nil end
  end

  # true for operators that are associative and commutative
  #
  def SEBinary.commutative?(op)
    op == '+' || op == '*'
  end

  def SEBinary.fold(op, *args)
    args = args.map { |arg|
      if arg.kind_of?(Integer) then SEInt.new(arg) else arg end
    }
    if SEBinary.commutative?(op)
      args = args.select { |e| e.constant? } + args.select { |e| ! e.constant? }
    end
    fst = args.shift
    raise Exception.new("SEbinary#fold: no arguments") unless fst
    args.inject(fst) { |a,b|
      SEBinary.create(op,a,b)
    }
  end

  # optimized creation for commutative operators
  #
  def SEBinary.collect_fold(op, *args)
    assert("SEBinary#collect_fold: #{op} is not commutative") { SEBinary.commutative?(op) }
    todo, done = args, []
    while ! todo.empty?
      expr = todo.pop
      if expr.kind_of?(SEBinary) && expr.op == op
        todo.push(expr.a)
        todo.push(expr.b)
      else
        done.push(expr)
      end
    end
    r = SEBinary.fold(op, *done)
    r
  end

  def to_s; "(#{a.to_s} #{op} #{b.to_s})"; end

  def eval(env,lenv = {})
    ae, be = [@a,@b].map { |expr| expr.eval(env,lenv) }
    case @op
    when '+'    then ae + be
    when '*'    then ae * be
    when 'umax' then [ae,be].max
    when 'smax' then [ae,be].max
    when 'umin' then [ae,be].min
    when 'smin' then [ae,be].min
    when '/u'   then ae / be
    when '/s'   then ae / be
    else        raise Exception.new("SymbolicExpression#eval: unknown binary operator #{@op}")
    end
  end

  def resolve_loops(lenv)
    an,bn = [@a,@b].map { |expr| expr.resolve_loops(lenv) }
    SEBinary.create(@op, an, bn)
  end

  def map_names(&block)
    SEBinary.create(@op, @a.map_names(&block), @b.map_names(&block))
  end

  def referenced_vars
    @a.referenced_vars + @b.referenced_vars
  end

  def referenced_loops
    @a.referenced_loops + @b.referenced_loops
  end
end

class SEAffineRec < SymbolicExpression

  attr_reader :loopheader

  def initialize(a,b,loopheader,flags='')
    @a, @b, @loopheader, @flags = a, b, loopheader, flags
  end

  def to_s; "{#{@a},+,#{@b}}<#{@flags}><#{@loopheader}>"; end

  def eval(env, lenv = nil)
    raise Exception.new("SEAFfineRec#eval: no loop environment given") unless lenv
    resolve_loops(lenv).eval(env,lenv)
  end

  def resolve_loops(lenv)
    loop_bound(lenv)
  end

  # Calculate loop bound given the loop bound of loops this recurrence depends on.
  # If x is the maximum trip count of the dependent loop, the expression
  # +smax(0, a + b * i)+ takes its maximum at
  #     a + b * (x-1) | b >= 0
  #     a             | b < 0
  def loop_bound(lenv)
    raise NoLoopBoundAvailableException.new("No loop bound for outer loop",@loopheader) unless lenv[@loopheader]
    ub = @a + @b * (lenv[@loopheader] - 1)
    @a.smax(0).smax(ub)
  end

  # Calculate the execution frequency of the loop bounded by this recurrence
  # relative to the referenced loop's entry-edges (i.e., its preheader).
  # Typical application are total bounds for triangle loops.
  #
  # == Computation of loop bound sums
  #
  # First assume that this recurrence specifies the loop bound for Lj,
  # and that the loop referenced by this recurrence Li is the parent loop
  # of Lj.
  #
  # The maximum trip count of Lj is +smax(0, a + b * i)+, where i
  # is the current iteration of the loop Li.
  #
  # The closed form for the number of time the header of Lj is executed
  # relative to the entry-edge frequency of the referenced loop Li is thus
  #
  #   (1) smax(0,a+b*i)[0..xi)
  #       where xi is the maximum trip count of Li
  #
  # In the following, we require that +b+ is constant.
  #
  # The expression +a+b*i+ is positive if
  #
  #   (2) i >= (-a /r b) | b >= 0
  #       i <= (a /r -b) | b <  0 (this case is treated seperately in (3a..6a))
  #
  # which is the case if
  #
  #   (3) i >= ceil(-a /r b) = (-a+b-1 /s b) = lb
  #
  # and thus we can get rid of the maximum to obtain
  #
  #   (4) (a+b*i)[lb..x)
  #       where lb = max((b-1-a) /s b),0)
  #
  # Now first assume that +x >= lb >= 0+; then
  #
  #   (5) (a+b*i)[lb..x) = (a+b*i)[0..x) - (a+b*i)[0..lb)
  #                      = (x-lb)*a + x*(x-1)*b /s 2 - lb*(lb-1)*b /s 2
  #
  # Next we deal the special case x < lb (which should result in 0)
  #
  #   (6) max(x-lb, 0) * a + max(x*max(x-1,0) - lb*(lb-1), 0) /u 2 * b
  #
  # This is the same as (5) for +x >= lb >= 0+; for +x < lb+ we get 0 as expected
  #
  # Now for +b < 0+:
  #
  #   (3a) i <= a /r -b <= (a-b-1 /s -b)
  #   (4a) (a+b*i)[0..end)
  #        where ub  = ((a-b-1) /s -b) + 1
  #              end = max(0,min(ub,x)))
  #   (5a) (a+b*i)[0..end) = end * a + end*(end-1) /u 2 * b (note that end >= 0)
  #
  # == Nested Independent Loops
  #
  # Note that +loop_bound_sum+ only provides the correct number of iterations
  # if Li is the direct parent of Lj in the loop nest tree.
  #
  # If the loop nest tree has a path
  #  Li -> L1 -> ... -> Ln -> Lj
  #
  # then the execution frequency of Lj's header block relative to the
  # frequency of Li's entry edges (or Li's preheader, if it exists) is
  #
  #      (7) x1 * ... * xn * smax(0,a+b*i)[0..xi)
  #          where x1,...,xn,xi is the maximum trip count of L1,...,Ln,Li
  def loop_bound_sum(outer_loop_bound)
    x = outer_loop_bound
    if ! @b.constant? || @b.to_i == 0
      raise Exception.new("SEAffineRec#loop_bound_sum: not possible to calculate total bound for"+
                          "non-constant/zero #{@b}::#{@b.class} in #{self}")
    end
    if @b.to_i > 0
      lb = @b.add(-@a,-1).sdiv(@b).smax(0)
      p1 = (x-lb).smax(0) * @a
      q1 = x * (x-1).smax(0)
      q2 = lb * (lb-1)
      p2 = (q1-q2).smax(0).sdiv(2) * @b
      p1+p2
    elsif @b.to_i < 0
      ub = @a.add(-@b,-1).sdiv(-@b).add(1)
      ev = ub.smin(x).smax(0)
      p1 = ev * @a
      p2 = (ev * (ev-1)).sdiv(2) * @b
      p1 + p2
    end
  end

  def referenced_loops
    s = @a.referenced_loops + @b.referenced_loops
    s.add(@loopheader)
    s
  end

  def referenced_vars
    @a.referenced_vars + @b.referenced_vars
  end

  def map_names(&block)
    ma, mb = [@a,@b].map { |v| v.map_names(&block) }
    mlh = block.call(:loop,@loopheader)
    SEAffineRec.new(ma, mb, mlh, @flags)
  end

  def chainOfRecurrences?; true; end
end

class SEUnknown < SymbolicExpression
  def initialize(str)
    @str = str
  end
  def to_s; "#{@str}"; end
  def map_names(&block); self; end
end

class SymbolicExpressionParser
  include Rsec::Helpers
  attr_reader :parser
  def initialize
    @parser = get_parser
  end
  def SymbolicExpressionParser.parse(expr_spec)
    return SEInt.new(expr_spec) if(expr_spec.kind_of?(Integer))
    p = SymbolicExpressionParser.new.parser
    begin
      p.parse!(expr_spec)
    rescue Rsec::SyntaxError
      warn("Unsupported symbolic bound #{expr_spec}")
      SEUnknown.new(expr_spec)
    end
  end

private
  def get_parser
    expr
  end
  def expr
    (rchain | composite_expr | variable | int)
  end
  def rchain
    spec = paren(seq(lazy{expr},sym(','),sym('+'),sym(','),lazy{expr}),'{','}').map { |a,_,_,_,b|
      [a,b]
    }
    flags = paren(flag,'<','>')
    loop = paren(loopname,'<','>')
    seq(spec,flags *(0..5),loop).map { |s,f,l|
      a,b = s
      SEAffineRec.new(a,b,l,f)
    }
  end
  def composite_expr
    arithop = one_of_("+*") | sym('umax') | sym('smax') | sym('/u') | sym('/s')
    paren(lazy {expr}.join(arithop)).map { |ps|
      stack = []
      last_op = nil
      while(ps.length>1)
        a  = ps.shift
        op = ps.shift
        stack.push([a,op])
        raise Exception.new("SymbolicExpressionParser: mixed ops without parenthesis") unless ! last_op || op == last_op
        last_op = op
      end
      expr = ps.first
      while !stack.empty?
        a,op = stack.pop
        expr = SEBinary.create(op,a,expr)
      end
      expr
    }
  end
  def paren(p,left='(',right=')')
    left.r >> p << right.r
  end
  def flag
    /n[us]?w/.r
  end
  def sym(c)
    symbol(c.r)
  end
  def int
    /-?\d+/.r.map { |v| SEInt.new(v.to_i) }
  end
  def loopname
    /[%@A-Za-z_][A-Za-z\.0-9_]*/.r
  end
  def variable
    loopname.map { |v| SEVar.new(v) }
  end
end

end # module PML

