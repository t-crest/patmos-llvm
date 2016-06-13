#
# platin tool set
#
# ILP module
#
require 'core/utils'
require 'core/pml'
require 'set'
module PML

class UnknownVariableException < Exception
  def initialize(msg)
    super(msg)
  end
end

class InconsistentConstraintException < Exception
  def initialize(msg)
    super(msg)
  end
end


# Indexed Constraints (normalized, with fast hashing)
# Terms: Index => Integer != 0
# Rhs: Integer
# Invariant: gcd(lhs.map(:second) + [rhs]) == 1
class IndexedConstraint
  attr_reader :name, :lhs, :op, :rhs, :key, :hash, :tags
  def initialize(ilp, lhs, op, rhs, name, tags=[])
    @ilp = ilp
    @name, @lhs, @op, @rhs = name, lhs, op, rhs
    @tags = tags
    raise Exception.new("add_indexed_constraint: name is nil") unless name
    assert("unexpected op #{@op}") { %w{equal less-equal}.include? @op }
    normalize!
  end
  def tautology?
    normalize! if @tauto.nil?
    @tauto
  end
  def inconsistent?
    normalize! if @inconsistent.nil?
    @inconsistent
  end
  # Check if this constraint has the form 'x <= c' or 'x >= c'
  def bound?
    normalize! if @inconsistent.nil?
    return false if @op != 'less-equal' or @lhs.length != 1
    v,c = @lhs.first
    c == -1 || c == 1
  end
  # Check if this constraint has the form '-x <= 0'
  def non_negative_bound?
    normalize! if @inconsistent.nil?
    return false if @op != 'less-equal' or @lhs.length != 1 or @rhs != 0
    v,c = @lhs.first
    c == -1 
  end
  def get_coeff(v)
    @lhs[v]
  end
  def named_lhs
    named_lhs = Hash.new(0)
    @lhs.each { |vi,c| named_lhs[@ilp.var_by_index(vi)] = c }
    named_lhs
  end
  def set(v,c)
    @lhs[v] = c
    invalidate!
  end
  def add(v,c)
    set(v,c+@lhs[v])
  end
  def invalidate!
    @key, @hash, @gcd, @tauto, @inconsistent = nil, nil, nil, nil,nil
  end
  def normalize!
    return unless @tauto.nil?
    @lhs.delete_if { |v,c| c == 0 }
    @tauto, @inconsistent = false, false
    if(@lhs.empty?)
      if @rhs == 0
        @tauto = true
      elsif @rhs >= 0 && @op == "less-equal"
        @tauto = true
      else
        @inconsistent = true
      end
    elsif @lhs.length == 1 && @op == "equal" && @rhs == 0
      # c != 0 -> c x = 0 <=> x = 0
      v, c = @lhs.first
      @lhs[v] = 1
    else
      @gcd = @lhs.values.inject(@rhs, :gcd)
      @lhs.merge!(@lhs) { |v,c| c / @gcd }
      @rhs /= @gcd
    end
  end
  def hash
    @hash if @hash
    @hash = key.hash
  end
  def key
    @key if @key
    normalize!
    @key = [@lhs,@op == 'equal',@rhs]
  end
  def ==(other)
    key == other.key
  end
  def <=>(other)
    key <=> other.key
  end
  def eql?(other); self == other ; end
  def inspect
    "Constraint#<#{lhs.inspect},#{@op.inspect},#{rhs.inspect}>"
  end
  def to_s(use_indices=false)
    lhs, rhs = Hash.new(0), Hash.new(0)
    (@lhs.to_a+[[0,-@rhs]]).each { |v,c|
      if c > 0
        lhs[v] += c
      else
        rhs[v] -= c
      end
    }
    [lhs.to_a, rhs.to_a].map { |ts|
      ts.map { |v,c|
        if v == 0
          (c==0) ? nil : c
        else
          vname = use_indices ? v.to_s : @ilp.var_by_index(v).to_s
          (c == 1) ? vname : "#{c} #{vname}"
        end
      }.compact.join(" + ")
    }.map { |s| s.empty? ? "0" : s }.join(@op == "equal" ? " = " : " <= ")
  end
end


# ILP base class (FIXME)
class ILP
  attr_reader :variables, :constraints, :costs, :options, :vartype, :solvertime
  # variables ... array of distinct, comparable items
  def initialize(options = nil)
    @solvertime = 0
    @options = options
    @variables = []
    @indexmap = {}
    @vartype = {}
    @eliminated = Hash.new(false)
    @constraints = Set.new
    reset_cost
  end
  # number of non-eliminated variables
  def num_variables
    variables.length - @eliminated.length
  end
  # short description
  def to_s
    "#<#{self.class}:#{num_variables} vars, #{constraints.length} cs>"
  end
  # print ILP
  def dump(io=$stderr)
    io.puts("max " + costs.map { |v,c| "#{c} #{v}" }.join(" + "))
    @indexmap.each do |v,ix|
      next if @eliminated[ix]
      io.puts " #{ix}: int #{v}"
    end
    @constraints.each_with_index do |c,ix|
      io.puts " #{ix}: constraint #{c.name}: #{c}"
    end
  end
  # index of a variable
  def index(variable)
    @indexmap[variable] or raise UnknownVariableException.new("unknown variable: #{variable}")
  end
  # variable indices
  def variable_indices
    @indexmap.values
  end
  # variable by index
  def var_by_index(ix)
    @variables[ix-1]
  end
  # set cost of all variables to 0
  def reset_cost
    @costs = Hash.new(0)
  end
  # get cost of variable
  def get_cost(v)
    @costs[v]
  end
  # remove all constraints
  def reset_constraints
    @constraints = Set.new
  end
  # add cost to the specified variable
  def add_cost(variable, cost)
    @costs[variable] += cost
    debug(@options, :ilp) { "Adding costs #{variable} = #{cost}" }
  end

  # add a new variable, if necessary
  def has_variable?(v)
    ! @indexmap[v].nil?
  end

  # add a new variable
  def add_variable(v, vartype= :machinecode)
    raise Exception.new("Duplicate variable: #{v}") if @indexmap[v]
    assert("ILP#add_variable: type is not a symbol") { vartype.kind_of?(Symbol) }
    debug(@options, :ilp) { "Adding variable #{v} :: #{vartype.inspect}" }

    @variables.push(v)
    index = @variables.length # starting with 1
    @indexmap[v] = index
    @vartype[v] = vartype
    @eliminated.delete(v)
    add_indexed_constraint({index => -1},"less-equal",0,"non_negative_v_#{index}",Set.new([:positive]))
    index
  end
  # add constraint:
  # terms_lhs .. [ [v,c] ]
  # op        .. "equal" or "less-equal"
  # const_rhs .. integer
  def add_constraint(terms_lhs,op,const_rhs,name,tag)
    assert("Markers should not appear in ILP") {
      ! terms_lhs.any? { |v,c| v.kind_of?(Marker) }
    }
    terms_indexed = Hash.new(0)
    terms_lhs.each { |v,c|
      terms_indexed[index(v)] += c
    }
    c = add_indexed_constraint(terms_indexed,op,const_rhs,name,Set.new([tag]))
    debug(options, :ilp) { "Adding constraint: #{terms_lhs} #{op} #{const_rhs} ==> #{c}" }
    c
  end

  # conceptually private; friend VariableElimination needs access
  def create_indexed_constraint(terms_indexed, op, const_rhs, name, tags)
    terms_indexed.default=0
    constr = IndexedConstraint.new(self, terms_indexed, op, const_rhs, name, tags)
    return nil if constr.tautology?
    raise InconsistentConstraintException.new("Inconsistent constraint #{name}: #{constr}") if constr.inconsistent?
    constr
  end

  private
  def add_indexed_constraint(terms_indexed, op, constr_rhs, name, tags)
    constr = create_indexed_constraint(terms_indexed, op, constr_rhs, name, tags)
    @constraints.add(constr) if constr
    constr
  end
end

end # module PML
