#
# PSK tool set
#
# ILP/IPET module
#
require 'utils'
begin
  require 'rubygems'
  require "lpsolve"
rescue Exception => details
  warn "Failed to load library lpsolve"
  info "  ==> aptitude install liblpsolve55-dev [Debian/Ubuntu]"
  info "  ==> gem1.9.1 install lpsolve --pre"
  die "Failed to load required ruby libraries"
end

module PML

# pretty print constraint (XXX: hackish)
def show_constraint(terms_lhs,op,const_rhs)
  lhs, rhs = Hash.new(0), Hash.new(0)
  (terms_lhs+[[:__const,-const_rhs]]).each { |v,c|
    if c > 0
      lhs[v] += c
    else
      rhs[v] -= c
    end
  }
  [lhs.to_a, rhs.to_a].map { |ts|
    ts.map { |v,c|
      if v == :__const && c == 0
        nil
      elsif v == :__const
        c
      elsif c == 1
        v
      else
        "#{c} #{v}"
      end
    }.compact.join(" + ")
  }.map { |s| s.empty? ? "0" : s }.join(op == "equal" ? " = " : " <= ")
end

class UnknownVariableException < Exception
  def initialize(msg)
    super(msg)
  end
end

# ILP base class
class ILP
  attr_reader :variables, :constraints, :cost, :options, :vartype
  # variables ... array of distinct, comparable items
  def initialize(options = nil)
    @options = options || OpenStruct.new(:verbose=>false,:debug=>false)
    @variables = []
    @indexmap = {}
    @vartype = {}
    @eliminated = Hash.new(false)
    @constraints = []
    @constraint_set = Hash.new(false)
    reset_cost
  end
  def to_s
    "#<#{self.class}:#{variables.length - @eliminated.length} vars, #{constraints.length} cs>"
  end
  # add a new variable
  def add_variable(v, vartype=:dst, lb = 0, ub = nil)
    $dbgs.puts "Adding variable #{v}" if @options.debug
    raise Exception.new("Duplicate variable: #{v}") if @indexmap[v]
    @variables.push(v)
    index = @variables.length # starting with 1
    @indexmap[v] = index
    @vartype[v] = vartype
    @eliminated.delete(v)
    add_bound(v,lb,ub)
    index
  end
  # index of a variable
  def index(variable)
    @indexmap[variable] or raise UnknownVariableException.new("unknown variable: #{variable}")
  end
  # set cost of all variables to 0
  def reset_cost
    @costs = Hash.new(0)
  end
  def get_cost(v)
    @costs[v]
  end
  # add cost to the specified variable
  def add_cost(variable, cost)
    # puts "Adding cost #{variable} -> #{cost}"
    @costs[variable] += cost
  end
  def reset_constraints
    @constraints, @constraint_set = [], Hash.new(false)
  end
  # add constraint:
  # terms_lhs .. [ [v,c] ]
  # op        .. "equal" or "less-equal"
  # const_rhs .. integer
  def add_constraint(terms_lhs,op,const_rhs,name=nil)
    name = "__#{@constraints.size}" unless name
    $dbgs.puts "Adding constraint #{show_constraint(terms_lhs,op,const_rhs)} (#{name})" if @options.debug
    terms_indexed = Hash.new(0)
    terms_lhs.each { |v,c|
      terms_indexed[index(v)] += c
    }
    add_indexed_constraint(terms_indexed,op,const_rhs,name)
  end
  def add_bound(variable,lb=nil,ub=nil)
    ix = index(variable)
    add_constraint([[variable,-1]],"less-equal",-lb,"__lower_bound_v#{ix}") if lb
    add_constraint([[variable,1]],"less-equal",ub,"__upper_bound_v#{ix}") if ub
  end
  # Fourier/Motzkin elimination
  def eliminate(var)
    raise Exception.new("ILP#eliminate: non-zero cost") if @costs[var] != 0
    var = index(var)
    # group all constraints into 4 groups
    # N: var does not occur in group
    # E: k var + ...  = c
    # L: k var + ... <= c with k > 0
    # U: k var + ... <= c with l > 0
    n,e,l,u = [],[],[],[]
    constraints_grouped = {}
    constraints.each do |constr|
      name,terms,op,const_rhs = constr
      coeff = terms.find { |v,c| v == var}
      coeff = coeff[1] if coeff
      assert("eliminate: 0-coeff not permitted") { !coeff || coeff != 0 }
      if ! coeff
        n.push(constr)
        next
      end
      terms.delete(var)
      set = if op == "equal"
              e
            elsif coeff > 0
              l
            else
              u
            end
      set.push([coeff,constr])
    end
    reset_constraints
    n.each { |name,terms,op,rhs| add_indexed_constraint(terms,op,rhs,name) }
    # substitution if E is non-empty
    if ! e.empty?
      e_coeff, e_constr = e.first
      _,e_terms,e_op,e_rhs = e_constr
      if e_coeff < 0
        e_coeff = -e_coeff
        e_terms.merge!(e_terms) { |v,c| 0-c }
        e_rhs = -e_rhs
      end
      (e[1..-1] + l + u).each do |coeff,constr|
        name,terms,op,rhs = constr
        # substitution: e_coeff terms' - coeff e_terms <=> e_coeff rhs - coeff e_rhs
        # (1) multiply by e_coeff
        terms = terms.merge!(terms) { |v,c| c * e_coeff }
        rhs   = rhs * e_coeff
        # (2) subtract (coeff * e_terms) and (coeff * e_rhs)
        e_terms.each { |v,c| terms[v] -= c * coeff }
        rhs -= coeff * e_rhs
        # (3) normalize
        terms.delete_if { |v,c| c == 0 }
        add_indexed_constraint(terms,op,rhs,name)
      end
    elsif l.empty? || u.empty?
      # nothing to do
    else
      # l: ax  + by <= d [ a  > 0 ]
      # u: a'x + cz <= e [ a' < 0 ]
      # a cz - a' by <= a e - a' d
      l.each do |l_coeff,l_constr|
        l_name, l_terms, l_op, l_rhs = l_constr
        u.each do |u_coeff,u_constr|
          u_name, u_terms, u_op, u_rhs = u_constr
          # terms = l_coeff * u_terms - u_coeff * l_terms
          terms = Hash.new(0)
          u_terms.each { |v,c| terms[v] += l_coeff * c }
          l_terms.each { |v,c| terms[v] -= u_coeff * c }
          rhs = l_coeff * u_rhs - u_coeff * l_rhs
          name = l_name+"-lt-"+u_name
          add_indexed_constraint(terms,u_op,rhs,name)

          # puts "Ran FM"
          # sl_terms = l_terms.dup ; sl_terms[var] = l_coeff
          # su_terms = u_terms.dup ; su_terms[var] = u_coeff
          # puts "L: #{show_constraint(sl_terms,l_op,l_rhs)}"
          # puts "U: #{show_constraint(su_terms,u_op,u_rhs)}"
          # puts "R: #{show_constraint(terms,u_op,rhs)} (#{name})"
        end
      end
    end
    @eliminated[var] = true
  end
  private
  def add_indexed_constraint(terms_indexed,op,const_rhs,name)
    terms_indexed.delete_if { |v,c| c == 0 }
    if(terms_indexed.empty?)
      return if const_rhs == 0
      return if const_rhs >= 0 && op == "less-equal"
      raise Exception.new("Inconsistent constraint: #{terms_lhs} #{op} #{const_rhs}")
    end
    div = terms_indexed.values.inject(0,:gcd)
    terms_indexed.merge!(terms_indexed) { |v,c| c / div }
    const_rhs /= div
    key = [terms_indexed,op,const_rhs]
    return if @constraint_set[key]
    @constraint_set[key] = true
    @constraints.push([name,terms_indexed,op,const_rhs])
  end
end
# Simple interface to lp_solve
class LpSolveILP < ILP
  # Tolarable floating point error in objective
  EPS=0.0001
  def initialize(options = nil)
    super(options)
    @eps = EPS
  end
  # run solver to find maximum cost
  def solve_max
    # create LP problem (maximize)
    lp = create_lp
    lp.set_maxim
    # set objective and add constraints
    lp.set_add_rowmode(true)
    set_objective(lp)
    add_linear_constraints(lp)
    # solve
    lp.set_add_rowmode(false)
    lp.print_lp if options.lp_debug
    lp.set_verbose(0)
    r = lp.solve
    # read solution
    lp.print_solution(-1) if options.lp_debug
    obj = lp.objective
    freqmap = extract_frequencies(lp.get_variables)
    if (r == LPSolve::INFEASIBLE)
      diagnose_infeasibility(freqmap)
    end
    lp_solve_error(r) unless r == 0
    if (obj-obj.round.to_f).abs > @eps
      raise Exception.new("Untolerable floating point inaccuracy > #{EPS} in objective #{obj}")
    end
    [obj.round, freqmap ]
  end

  private
  # create an LP with variables
  def create_lp
    lp = LPSolve.new(0, variables.size)
    variables.each do |v|
      ix = index(v)
      lp.set_col_name(ix, "v_#{ix}")
      lp.set_int(ix, true)
    end
    lp
  end
  # set LP ovjective
  def set_objective(lp)
    lp.set_obj_fnex(@costs.map { |v,c| [index(v),c] })
  end
  # add LP constraints
  def add_linear_constraints(lp)
    @constraints.each do |name,terms,op,const_rhs|
      lp.add_constraintex(name,terms.to_a,lpsolve_op(op),const_rhs)
    end
  end
  # extract solution vector
  def extract_frequencies(fs)
    vmap = {}
    fs.each_with_index do |v, ix|
      vmap[@variables[ix]] = v
    end
    vmap
  end
  # lp-solve comparsion operators
  def lpsolve_op(op)
    case op
    when "equal"
      LPSolve::EQ
    when "less-equal"
      LPSolve::LE
    when "greater-equal"
      LPSolve::GE
    else
      internal_error("Unsupported comparison operator #{op}")
    end
  end
  def lp_solve_error(r)
    msg =
      case r
      when LPSolve::NOMEMORY
        "NOMEMORY"
      when LPSolve::SUBOPTIMAL
        "SUBOPTIMAL"
      when LPSolve::INFEASIBLE
        "INFEASIBLE"
      when LPSolve::UNBOUNDED
        "UNBOUNDED"
      else
        "ERROR_#{r}"
      end
    raise Exception.new("LPSolver Error: #{msg} (E#{r})")
  end

  SLACK=10000000
  BIGM= 10000000
  def diagnose_infeasibility(freqmap)
    $stderr.puts "INFEASIBLE PROBLEM (#{@infeasible_diagnosis})"
    lp_solve_error(LPSolve::INFEASIBLE) if $loop==true
    $loop=true
    old_constraints, slackvars = @constraints, []
    reset_constraints
    variables.each do |v|
      add_constraint([[v,1]],"less-equal",BIGM,"__debug_upper_bound_v#{index(v)}")
    end
    old_constraints.each { |n,terms,op,rhs|
      next if n =~ /__positive_/
      v_lhs = add_variable("__slack_#{n}",:slack,0, BIGM)
      add_cost("__slack_#{n}", -SLACK)
      terms[v_lhs] = -1
      if op == "equal"
        v_rhs = add_variable("__slack_#{n}_rhs",:slack,0, BIGM)
        add_cost("__slack_#{n}_rhs", -SLACK)
        terms[v_rhs] = 1
      end
      add_indexed_constraint(terms,op,rhs,"__slack_#{n}")
    }
    @eps = 1.0
    #@constraints.each do |n,terms,op,rhs|
    #  puts "Slacked constraint #{n}: #{show_constraint(terms.map {|ix,c|[variables[ix-1],c]},op,rhs)}"
    #end
    cycles,freq = self.solve_max
    freq.each do |v,k|
      if v.to_s =~ /__slack/ && k != 0
        $stderr.puts "SLACK: #{v.to_s.ljust(40)} #{k.to_s.rjust(8)}"
      end
    end
    lp_solve_error(LPSolve::INFEASIBLE)
  end
end

class Callsite
  attr_reader :qname,:inst,:level
  def initialize(inst, level)
    @inst, @level = inst, level
    level_prefix = @level == :src ? "src/" : ""
    @qname = "#{level_prefix}/#{@inst.qname}"
  end
  def to_s ; @qname ; end
  def hash;  @qname.hash ; end
  def ==(other); qname == other.qname ; end
  def eql?(other); self == other; end
end

class Edge
  attr_reader :qname,:source,:target, :level
  def initialize(edge_source, edge_target, level)
    @source,@target,@level = edge_source, edge_target, level
    arrow  = @level == :src ? "~>" : "->"
    @qname = "#{@source.qname}#{arrow}#{:exit == @target ? 'exit' : @target.qname}"
  end
  def backedge?
    return false if :exit == target
    return false unless source.kind_of?(Block)
    return false unless source.loopnest >= target.loopnest && target.loopheader?
    source_loop_index = source.loopnest - target.loopnest
    r = source.loops[source_loop_index] == target
    r
  end
  def to_s ; @qname ; end
  def hash;  @qname.hash ; end
  def ==(other); qname == other.qname ; end
  def eql?(other); self == other; end
end


class IPETModel
  attr_reader :pml, :ipet, :level
  attr_reader :infeasible, :calltargets
  def initialize(ipet, level)
    @ipet, @level = ipet, level
    @calltargets = {}
    @infeasible = Set.new
  end

  def add_calltargets(cs, fs)
    @calltargets[cs] = fs
  end

  def set_infeasible(block)
    @infeasible.add(block)
  end

  def add_callsite(callsite, fs)
    # variable for callsite
    ipet.add_variable(callsite, level)
    # frequency of call instruction = frequency of block
    lhs = [ [callsite,1] ] + block_frequency(callsite.block,-1)
    ipet.add_constraint(lhs, "equal", 0, "callsite_#{callsite.qname}")

    # create call edges (callsite -> f) for each called function f
    # the sum of all calledge frequencies is equal to the callsite frequency
    calledges = []
    lhs = [ [callsite, 1] ]
    fs.each do |f|
      calledge = Edge.new(callsite, f, level)
      ipet.add_variable(calledge, level)
      calledges.push(calledge)
      lhs.push([calledge, -1])
    end
    ipet.add_constraint(lhs,"equal",0,"calledges_#{callsite.qname}")

    # return call edges
    calledges
  end

  # frequency of analysis entry is 1
  def add_entry_constraint(entry_function)
    ipet.add_constraint(function_frequency(entry_function),"equal",1,"entry")
  end

  # frequency of function is equal to sum of all callsite frequencies
  def add_function_constraint(function, calledges)
    lhs = calledges.map { |e| [e,-1] }
    lhs.concat(function_frequency(function,1))
    ipet.add_constraint(lhs,"equal",0,"callers_#{function}")
  end

  # frequency of incoming is frequency of outgoing edges
  def add_block_constraint(block)
    return if block.predecessors.empty?
    lhs = if block.successors.empty?
            lhs = sum_incoming(block,-1) + [[Edge.new(block,:exit,level),1]]
          else
            lhs = sum_incoming(block,-1) + sum_outgoing(block)
          end
    ipet.add_constraint(lhs,"equal",0,"structural_#{block.qname}")
  end

  def function_frequency(function, factor = 1)
    block_frequency(function.blocks.first, factor)
  end
  def block_frequency(block, factor=1)
    if block.successors.empty? # return exit edge
      [[Edge.new(block,:exit,level),factor]]
    else
      sum_outgoing(block,factor)
    end
  end
  def sum_incoming(block, factor=1)
    block.predecessors.map { |pred|
      [Edge.new(pred,block,level), factor]
    }
  end
  def sum_outgoing(block, factor=1)
    block.successors.map { |succ|
      [Edge.new(block,succ,level), factor]
    }
  end
  def sum_loop_entry(loopblock, factor=1)
    sum_incoming(loopblock,factor).reject { |edge,factor|
      edge.backedge?
    }
  end

  # returns all edges, plus all return blocks
  def each_edge(function)
    function.blocks.each do |bb|
      bb.successors.each do |bb2|
        yield Edge.new(bb,bb2,level)
      end
      if bb.successors.empty? # returns
        yield Edge.new(bb,:exit,level)
      end
    end
  end
end # end of class IPETBuilder

class IPETBuilder
  attr_reader :ilp
  def initialize(pml, options)
    @ilp     = LpSolveILP.new(options)
    @mc_model = IPETModel.new(@ilp, :dst)
    if options.use_relation_graph
      @bc_model = IPETModel.new(@ilp, :src)
    end
    @ffcount = 0
    @pml, @options = pml, options
  end

  def add_calltargets(cs, fs, level)
    # FIXME: interprocedural analysis is only available for machinecode
    return unless level == :dst
    @mc_model.add_calltargets(cs,fs)
  end

  def set_infeasible(block, level)
    model = (level == :dst) ? (@mc_model) : (@bc_model)
    model.set_infeasible(block)
  end

  # Build basic IPET structure.
  # Yields blocks, so the caller can compute their cost
  def build(entry)
    functions = reachable_set(entry) do |function|
      succs = Set.new
      function.each_callsite { |cs|
        next if @mc_model.infeasible.include?(cs.block)
        cs['callees'].each { |fname|
          if(fname == '__any__')
            # external flow fact
            @mc_model.calltargets[cs].each { |f| succs.add(f) }
          else
            # compiler information
            f = @pml.machine_functions.by_label(fname)
            @mc_model.add_calltargets(cs, [f])
            succs.add(f)
          end
        }
      }
      add_bitcode_variables(function) if @bc_model
      @mc_model.each_edge(function) do |edge|
        @ilp.add_variable(edge, :dst)
        cost = yield edge
        @ilp.add_cost(edge, cost)
      end
      succs # return successors to reachable_set
    end
    function_callers = {}
    functions.each do |f|
      add_bitcode_constraints(f) if @bc_model
      f.blocks.each do |block|
        @mc_model.add_block_constraint(block)
        next if @mc_model.infeasible.include?(block)
        block.callsites.each do |cs|
          call_edges = @mc_model.add_callsite(cs, @mc_model.calltargets[cs])
          call_edges.each do |ce|
            (function_callers[ce.target] ||= []).push(ce)
          end
        end
      end
    end
    @mc_model.add_entry_constraint(entry)
    function_callers.each do |f,ces|
      @mc_model.add_function_constraint(f, ces)
    end
  end


  # currently we only support block frequency bounds
  def add_flowfact(ff)
    scope, bref, freq = ff.get_block_frequency_bound
    model = ff.level == "machinecode" ? @mc_model : @bc_model
    raise Exception.new("IPETBuilder#add_flowfact: cannot add bitcode flowfact without using relation graph") unless model
    lhs = []
    if scope.kind_of?(FunctionRef)
      lhs = model.block_frequency(bref.block) + model.function_frequency(scope.function, -freq)
    elsif scope.kind_of?(LoopRef)
      lhs = model.block_frequency(bref.block) + model.sum_loop_entry(scope.loopblock, -freq)
    else
#      $stderr.puts "Skipping unsupported constraint #{ff}"
      return false
    end
    begin
      ilp.add_constraint(lhs,"less-equal", 0, "#{ff['classification']}_#{@ffcount+=1}")
    rescue Exception => detail
      $stderr.puts "Skipping constraint: #{detail}"
    end
  end

private
  # add variables for bitcode basic blocks and relation graph
  # (only if relation graph is available)
  def add_bitcode_variables(machine_function)
    return unless @pml.relation_graphs.has_named?(machine_function.name, :dst)
    rg = @pml.relation_graphs.by_name(machine_function.name, :dst)
    bitcode_function = rg.get_function(:src)
    @bc_model.each_edge(bitcode_function) do |edge|
      @ilp.add_variable(edge, :src)
    end
    each_relation_edge(rg) do |edge|
      @ilp.add_variable(edge, :rel)
    end
  end

  # add constraints for bitcode basic blocks and relation graph
  # (only if relation graph is available)
  def add_bitcode_constraints(machine_function)
    return unless @pml.relation_graphs.has_named?(machine_function.name, :dst)
    rg = @pml.relation_graphs.by_name(machine_function.name, :dst)
    bitcode_function = rg.get_function(:src)
    bitcode_function.blocks.each { |block|
      @bc_model.add_block_constraint(block)
    }
    # group relation edges by corresponding BC/MC edge and by source node
    rg_edges_of_edge   = { :src => {}, :dst => {} }
    rg_edges_by_source = {}
    each_relation_edge(rg) do |edge|
      level = edge.level
      source_block = edge.source.get_block(level)
      target_block = (edge.target.type == :exit) ? :exit : (edge.target.get_block(level))

      assert("Bad RG: #{edge}") { source_block && target_block }

      if [:entry,:progress].include?(edge.source.type)
        rg_edges_by_source[edge.source] ||= { :src => [], :dst => [] }
        rg_edges_by_source[edge.source][level].push(edge)
      end
      (rg_edges_of_edge[level][Edge.new(source_block,target_block,level)] ||=[]).push(edge)
    end
    rg_edges_of_edge.each do |level,edgemap|
      edgemap.each do |edge,rg_edges|
        @ilp.add_constraint(rg_edges.map {|rge|[rge,1]}+[[edge,-1]], "equal", 0, "rg_edge_#{edge.qname}")
      end
    end
    rg_edges_by_source.each do |s,edges|
      lhs = edges[:src].map { |e| [e,1] } + edges[:dst].map { |e| [e,-1] }
      @ilp.add_constraint(lhs, "equal", 0, "rg_progress_#{s.qname}")
    end
  end

  # return all relation-graph edges
  def each_relation_edge(rg)
    rg.nodes.each { |node|
      [:src,:dst].each { |level|
        next unless node.get_block(level)
        node.successors(level).each { |node2|
          if node2.type == :exit || node2.get_block(level)
            yield Edge.new(node,node2,level)
          end
        }
      }
    }
  end

end

end # module PML
