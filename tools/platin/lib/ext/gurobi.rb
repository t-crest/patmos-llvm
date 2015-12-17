#
# PLATIN tool set
#
# Bindings for Gurobi
#
require 'platin'
include PML

# Simple interface to gurobi_cl
class GurobiILP < ILP
  # Tolarable floating point error in objective
  def initialize(options = nil)
    super(options)
  end
  # run solver to find maximum cost
  def solve_max
    # create LP problem (maximize)
    lp_name = options.write_lp
    lp_name = File.join(options.outdir, "model.lp") unless lp_name
    sol_name = File.join(options.outdir, "model.sol")
    lp = File.open(lp_name, "w")

    # set objective and add constraints
    add_objective(lp)
    add_linear_constraints(lp)
    add_variables(lp)

    lp.close

    # solve
    debug(options, :ilp) { self.dump(DebugIO.new) }
    start = Time.now
    err = solve_lp(lp_name, sol_name)
    @solvertime += (Time.now - start)

    # Throw exception on error (after setting solvertime)
    if err
      gurobi_error(err)
    end

    # read solution
    sol = File.open(sol_name, "r")

    obj, freqmap = read_results(sol)

    # close temp files
    sol.close

    if obj.nil?
      gurobi_error("Could not read objective value from result file #{sol_name}")
    end
    if freqmap.length != @variables.length
      gurobi_error("Read #{freqmap.length} variables, expected #{@variables.length}")
    end

    [obj.round, freqmap ]
  end

  private
  # Remove characters from constraint names that are not allowed in an .lp file
  def cleanup_name(name)
    name.gsub(/[\@\: \/\(\)\-\>]/, "_")
  end
  def varname(vi)
    "v_#{vi}"
  end
  # create an LP with variables
  def add_variables(lp)
    lp.puts("Generals")
    @variables.each do |v|
      lp.print(" ", varname(index(v)))
    end
    lp.puts
    lp.puts("End")
  end
  # set LP ovjective
  def add_objective(lp)
    lp.puts("Maximize")
    @costs.each { |v,c| lp.print(" #{lp_term(c,index(v))}") }
    lp.puts
  end
  # add LP constraints
  def add_linear_constraints(lp)
    lp.puts("Subject To")
    @constraints.each do |constr|
      next if constr.bound?
      lp.puts(" #{cleanup_name(constr.name)}: #{lp_lhs(constr.lhs)} #{lp_op(constr.op)} #{constr.rhs}")
    end
    # We could put the bounds in as constraints, but bounds should be faster
    lp.puts("Bounds")
    @constraints.each do |constr|
      # Bounds must have the form '[-1,1] x <= rhs'
      next unless constr.bound?
      # All integer variables are bounded to [0,inf] by default
      next if constr.non_negative_bound?
      v,c = constr.lhs.first
      lp.puts(" #{varname(v)} >= #{constr.rhs}") if c == -1
      lp.puts(" #{varname(v)} <= #{constr.rhs}") if c ==  1
    end
    lp.puts
  end
  # extract solution vector
  def read_results(sol)
    obj = nil
    vmap = {}
    sol.readlines.each { |line|
      if line =~ /# Objective value = ([0-9][0-9.+e]*)/
	# Need to convert to float first, otherwise very large results that are printed in exp format
	# are truncated to the first digit.
        obj = $1.to_f.to_i
      elsif line =~ /v_([0-9]*) ([0-9]*)/
        vmap[var_by_index($1.to_i)] = $2.to_i
      end
    }
    [obj, vmap]
  end
  def lp_term(c,vi)
    "#{c < 0 ? '-' : '+'} #{c.abs} #{varname(vi)}"
  end
  def lp_lhs(lhs)
    lhs.map { |vi,c| " #{lp_term(c,vi)}" }.join
  end
  # lp comparsion operators
  def lp_op(op)
    case op
    when "equal"
      "="
    when "less-equal"
      "<="
    when "greater-equal"
      ">="
    else
      internal_error("Unsupported comparison operator #{op}")
    end
  end

  def solve_lp(lp, sol)
    out = IO.popen("gurobi_cl ResultFile=#{sol} #{lp}")
    lines = out.readlines
    # Detect error messages
    return "Gurobi terminated unexpectedly (#{$?.exitstatus})" if $?.exitstatus > 0
    lines.each do |line|
      return line if line =~ /Model is infeasible/
      return line if line =~ /Model is unbounded/
      return nil if line =~ /Optimal solution found/
    end
    nil # No error
  end
  def gurobi_error(msg)
    raise Exception.new("Gurobi Error: #{msg}")
  end
end
