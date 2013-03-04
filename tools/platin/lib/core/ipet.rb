#
# PSK tool set
#
# ILP/IPET/FM module
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

# Indexed Constraints (normalized, with fast hashing)
# Terms: Index => Integer != 0
# Rhs: Integer
# Invariant: gcd(lhs.map(:second) + [rhs]) == 1
class IndexedConstraint
  attr_reader :name, :lhs, :op, :rhs, :key, :hash
  def initialize(ilp, lhs, op, rhs, name)
    @ilp = ilp
    @name, @lhs, @op, @rhs = name, lhs, op, rhs
    raise Exception.new("add_indexed_constraint: name is nil") unless name
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
    else
      @gcd = @lhs.values.inject(0,:gcd)
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

# ILP base class
class ILP
  attr_reader :variables, :constraints, :costs, :options, :vartype
  # variables ... array of distinct, comparable items
  def initialize(options = nil)
    @options = options || OpenStruct.new(:verbose=>false,:debug=>false)
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
  # add a new variable
  def add_variable(v, vartype=:dst, lb = 0, ub = nil)
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
  # variable by index
  def var_by_index(ix)
    @variables[ix-1]
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
    @constraints = Set.new
  end
  # add constraint:
  # terms_lhs .. [ [v,c] ]
  # op        .. "equal" or "less-equal"
  # const_rhs .. integer
  def add_constraint(terms_lhs,op,const_rhs,name)
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

  # Substitution
  def eliminate_weak(var)
    eliminate(var,true)
  end

  # Fourier/Motzkin elimination
  def eliminate(var, weak=false)
    raise Exception.new("ILP#eliminate: non-zero cost") if @costs[var] != 0
    var = index(var)
    # group all constraints into 4 groups
    # N: var does not occur in group
    # E: k var + ...  = c
    # L: k var + ... <= c with k > 0
    # U: k var + ... <= c with k < 0
    e,l,u = [],[],[]
    # $stderr.puts("FM/Elimination: starting with a set of #{@constraints.length}")
    @constraints.reject! do |constr|
      coeff = constr.get_coeff(var)
      if ! coeff || coeff == 0
        # not affected, keep
        false
      else
        set = if constr.op == "equal"
                e
              elsif coeff > 0
                l
              else
                u
              end
        set.push([coeff,constr])
        # delete
        true
      end
    end
    # $dbgs.puts("FM/Elimination: set of e=#{e.length}, l=#{l.length}, u=#{u.length}")
    # Trivial: no constraints for var
    if e.empty? && l.empty? && u.empty?
      @eliminated[var] = true
      return
    end
    # No weak elimination possible
    if e.empty? && weak
      l.each { |_,c| @constraints.add(c) }
      u.each { |_,c| @constraints.add(c) }
      @eliminated[var] = false
      return
    end
    # Delete var
    (e+l+u).each { |coeff,constr|
      constr.add(var, -coeff)
    }
    # Elimination
    if ! e.empty?
      # Substitution if E is non-empty
      # FIXME: hack to avoid callsite constraints (for flow-fact trafo)
      eq_constr = e.find { |coeff,constr| constr.name !~ /^callsite_/ }
      eq_constr = e.first unless eq_constr
      e.delete(eq_constr)
      e_coeff, e_constr = eq_constr
      e_terms, e_rhs = e_constr.lhs, e_constr.rhs

      if e_coeff < 0
        e_coeff = -e_coeff
        e_terms.merge!(e_terms) { |v,c| 0-c }
        e_rhs = -e_rhs
      end
      (e + l + u).each do |coeff,constr|
        terms, rhs = constr.lhs, constr.rhs
        # substitution: e_coeff terms' - coeff e_terms <=> e_coeff rhs - coeff e_rhs
        # (1) multiply by e_coeff
        terms = terms.merge!(terms) { |v,c| c * e_coeff }
        rhs   = rhs * e_coeff
        # (2) subtract (coeff * e_terms) and (coeff * e_rhs)
        e_terms.each { |v,c| terms[v] -= c * coeff }
        rhs -= coeff * e_rhs
        # (3) add new constraint
        add_indexed_constraint(terms, constr.op, rhs, constr.name)
      end
    else
      assert("FM elimination should be disabled for eliminate_weak") { ! weak }
      # FM-Elimination
      # l: ax  + by <= d [ a  > 0 ]
      # u: a'x + cz <= e [ a' < 0 ]
      # a cz - a' by <= a e - a' d
      l.each do |l_coeff,l_constr|
        l_terms, l_rhs = l_constr.lhs, l_constr.rhs
        u.each do |u_coeff,u_constr|
          u_terms, u_rhs = u_constr.lhs, u_constr.rhs
          # terms = l_coeff * u_terms - u_coeff * l_terms
          terms = Hash.new(0)
          u_terms.each { |v,c| terms[v] += l_coeff * c }
          l_terms.each { |v,c| terms[v] -= u_coeff * c }
          rhs = l_coeff * u_rhs - u_coeff * l_rhs
          name = l_constr.name+"<>"+u_constr.name
          add_indexed_constraint(terms,l_constr.op,rhs,name)
        end
      end
    end
    @eliminated[var] = true
  end
  private
  def add_indexed_constraint(terms_indexed,op,const_rhs,name)
    constr = IndexedConstraint.new(self, terms_indexed, op, const_rhs, name)
    return if constr.tautology?
    raise Exception.new("Inconsistent constraint #{name}: #{constr}") if constr.inconsistent?
    @constraints.add(constr)
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


class IPETEdge
  attr_reader :qname,:source,:target, :level
  def initialize(edge_source, edge_target, level)
    @source,@target,@level = edge_source, edge_target, level
    arrow  = @level == :src ? "~>" : "->"
    @qname = "#{@source.qname}#{arrow}#{:exit == @target ? 'exit' : @target.qname}"
  end
  def backedge?
    return false if target == :exit
    target.backedge_target?(source)
  end
  def pml_entity?
    return false unless source.kind_of?(Block)
    return false unless :exit == target || target.kind_of?(Block)
    true
  end
  def ref
    assert("IPETEdge#ref: not a PML entity") { pml_entity? }
    if :exit == target
      BlockRef.new(source)
    else
      EdgeRef.new(source, target)
    end
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

  # XXX: suboptimal ad-hoc propagation of infeasibility
  # might lead to deep recursion
  def set_infeasible(block)
    @infeasible.add(block)
    block.successors.each { |bsucc|
      next if @infeasible.include?(bsucc)
      if bsucc.predecessors.all? { |bpred| @infeasible.include?(bpred) || bsucc.backedge_target?(bpred) }
        set_infeasible(bsucc)
      end
    }
    block.predecessors.each { |bpred|
      next if @infeasible.include?(bpred)
      if bpred.successors.all? { |bsucc| @infeasible.include?(bsucc) }
        set_infeasible(bpred)
      end
    }
  end

  # FIXME: we do not have information on predicated calls ATM.
  # Therefore, we use <= instead of = for call equations
  def add_callsite(callsite, fs)
    # variable for callsite
    ipet.add_variable(callsite, level)
    # frequency of call instruction = frequency of block
    lhs = [ [callsite,1] ] + block_frequency(callsite.block,-1)
    ipet.add_constraint(lhs, "equal", 0, "callsite_#{callsite.qname}")

    # create call edges (callsite -> f) for each called function f
    # the sum of all calledge frequencies is (less than or) equal to the callsite frequency
    # Note: less-than in the presence of predicated calls
    calledges = []
    lhs = [ [callsite, -1] ]
    fs.each do |f|
      calledge = IPETEdge.new(callsite, f, level)
      ipet.add_variable(calledge, level)
      calledges.push(calledge)
      lhs.push([calledge, 1])
    end
    ipet.add_constraint(lhs,"less-equal",0,"calledges_#{callsite.qname}")

    # return call edges
    calledges
  end

  # frequency of analysis entry is 1
  def add_entry_constraint(entry_function)
    ipet.add_constraint(function_frequency(entry_function),"equal",1,"structural_entry")
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
            lhs = sum_incoming(block,-1) + [[IPETEdge.new(block,:exit,level),1]]
          else
            lhs = sum_incoming(block,-1) + sum_outgoing(block)
          end
    ipet.add_constraint(lhs,"equal",0,"structural_#{block.qname}")
  end

  # frequency of incoming is frequency of outgoing edges is 0
  def add_infeasible_block(block)
    add_block_constraint(block)
    unless block.predecessors.empty?
      ipet.add_constraint(sum_incoming(block),"equal",0,"structural_#{block.qname}_0in")
    end
    unless block.successors.empty?
      ipet.add_constraint(sum_outgoing(block),"equal",0,"structural_#{block.qname}_0out")
    end
  end

  def function_frequency(function, factor = 1)
    block_frequency(function.blocks.first, factor)
  end
  def block_frequency(block, factor=1)
    if block.successors.empty? # return exit edge
      [[IPETEdge.new(block,:exit,level),factor]]
    else
      sum_outgoing(block,factor)
    end
  end
  def edgeref_frequency(edgeref, factor = 1)
    [[IPETEdge.new(edgeref.source, edgeref.target, level), factor ]]
  end
  def sum_incoming(block, factor=1)
    block.predecessors.map { |pred|
      [IPETEdge.new(pred,block,level), factor]
    }
  end
  def sum_outgoing(block, factor=1)
    block.successors.map { |succ|
      [IPETEdge.new(block,succ,level), factor]
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
        yield IPETEdge.new(bb,bb2,level)
      end
      if bb.successors.empty? # returns
        yield IPETEdge.new(bb,:exit,level)
      end
    end
  end
end # end of class IPETBuilder

class IPETBuilder
  attr_reader :ilp
  def initialize(pml, options, ilp = nil)
    @ilp = ilp
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
    mf_functions = reachable_set(entry[:dst]) do |mf_function|
      succs = Set.new
      mf_function.each_callsite { |cs|
        next if @mc_model.infeasible.include?(cs.block)
        if cs.unresolved_call?
            if ! @mc_model.calltargets[cs]
              die("Unknown calltargets for #{cs}")
            end
            # external flow fact
            @mc_model.calltargets[cs].each { |f| succs.add(f) }
        else
          cs.callees.each { |fname|
            # compiler information
            f = @pml.machine_functions.by_label(fname)
            @mc_model.add_calltargets(cs, [f])
            succs.add(f)
          }
        end
      }
      add_bitcode_variables(mf_function) if @bc_model
      @mc_model.each_edge(mf_function) do |edge|
        @ilp.add_variable(edge, :dst)
        cost = yield edge
        @ilp.add_cost(edge, cost)
      end
      succs # return successors to reachable_set
    end
    mf_function_callers = {}
    mf_functions.each do |f|
      add_bitcode_constraints(f) if @bc_model
      f.blocks.each do |block|
        if @mc_model.infeasible.include?(block)
          @mc_model.add_infeasible_block(block)
          next
        end
        @mc_model.add_block_constraint(block)
        block.callsites.each do |cs|
          call_edges = @mc_model.add_callsite(cs, @mc_model.calltargets[cs])
          call_edges.each do |ce|
            (mf_function_callers[ce.target] ||= []).push(ce)
          end
        end
      end
    end
    @mc_model.add_entry_constraint(entry[:dst])
    mf_function_callers.each do |f,ces|
      @mc_model.add_function_constraint(f, ces)
    end
  end

  #
  # Refine control-flow model using infeasible/calltarget flowfact information
  #
  # This method implements two refinements:
  #
  # (1) in scope main, frequency==0 => dead code (infeasible)
  # (2) in scope main, cs calls one of targets => refine calltarget sets
  #
  def refine(entry, flowfacts)
    flowfacts.each do |ff|
      # set indirect call targets
      model = (ff.level == "machinecode") ? :dst : :src
      scope,cs,targets = ff.get_calltargets
      if scope && scope.kind_of?(FunctionRef) && scope.function == entry[model]
        add_calltargets(cs.instruction, targets, model)
      end
      # set infeasible blocks
      scope,bref = ff.get_block_infeasible
      if scope && scope.kind_of?(FunctionRef) && scope.function == entry[model]
        set_infeasible(bref.block, model)
      end
    end
  end

  #
  # Add flowfacts
  #
  # Supported flowfacts:
  #
  # (1) Linear Combinations of Block or Edge Frequencies relative to Function or Loop Scope
  #
  def add_flowfact(ff)
    model = ff.level == "machinecode" ? @mc_model : @bc_model
    raise Exception.new("IPETBuilder#add_flowfact: cannot add bitcode flowfact without using relation graph") unless model
    lhs = []
    ff.lhs.each { |term|
      pp = term.ppref
      if pp.kind_of?(FunctionRef)
        lhs += model.function_frequency(pp.function, term.factor)
      elsif pp.kind_of?(BlockRef)
        lhs += model.block_frequency(pp.block, term.factor)
      elsif pp.kind_of?(EdgeRef)
        lhs += model.edgeref_frequency(pp, term.factor)
      elsif pp.kind_of?(InstructionRef)
        # XXX: exclusively used in refinement for now
        return false
      else
        die("Unknown reference type: #{pp.class}")
      end
    }
    scope = ff.scope
    if scope.kind_of?(FunctionRef)
      lhs += model.function_frequency(scope.function, -ff.rhs)
    elsif scope.kind_of?(LoopRef)
      lhs += model.sum_loop_entry(scope.loopblock, -ff.rhs)
    else
      $stderr.puts "Skipping unsupported constraint #{ff}"
      return false
    end
    begin
      name = "ff_#{ff['classification']}_#{@ffcount+=1}"
      ilp.add_constraint(lhs, ff.op, 0, name)
      name
    rescue UnknownVariableException => detail
      $stderr.puts "Skipping constraint: #{detail}" if @options.debug
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
      (rg_edges_of_edge[level][IPETEdge.new(source_block,target_block,level)] ||=[]).push(edge)
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
            yield IPETEdge.new(node,node2,level)
          end
        }
      }
    }
  end
end # IPETModel

class FlowFactTransformation

  attr_reader :pml, :options

  def initialize(pml,options)
    @pml, @options = pml, options
  end

  # Copy flowfacts
  def copy(flowfacts)
    copied = []
    flowfacts.each { |ff|
      ff2 = ff.deep_clone
      ff2.add_attribute('origin', options.flow_fact_output)
      info("Adding support for #{ff2.lhs.map { |t| t.ppref.function }.join(",")}") if options.verbose
      copied.push(ff2)
    }
    copied.each { |ff| pml.flowfacts.add(ff) }
    statistics("flowfacts copied to #{options.flow_fact_output}" => copied.length) if options.stats
  end
  def transform(machine_entry, flowfacts, target_level)
    ilp  = ILP.new
    builder_opts = options.dup
    builder_opts.use_relation_graph = true
    ipet = IPETBuilder.new(pml,builder_opts,ilp)
    entry = { :dst => machine_entry, :src => pml.bitcode_functions.by_name(machine_entry.label) }

    # Refine Control-Flow Model
    ipet.refine(entry, flowfacts)

    # Build IPET, no costs
    ipet.build(entry) { |edge| 0 }

    # Add flow facts
    flowfacts.each { |ff|
      name = ipet.add_flowfact(ff)
    }

    # If direction up/down, eliminate all vars but dst/src
    info "Running transformer to level #{target_level}" if options.verbose
    constraints_before = ilp.constraints.length
    ilp.variables.each do |var|
      if ilp.vartype[var] != target_level || var.kind_of?(Instruction) || ! var.pml_entity?
        ilp.eliminate(var)
      end
    end
    statistics("constraints after FM-elimination (#{constraints_before})" => ilp.constraints.length) if options.stats

    new_flowfacts = []
    ilp.constraints.each do |constr|
      lhs = constr.named_lhs
      name = constr.name

      next if name =~ /^__lower_bound/ && constr.rhs == 0
      next if name =~ /^structural/
      next if name =~ /^rg/
      # next if name =~ /^call/

      # Simplify: edges->block
      unless lhs.any? { |var,_| ! var.kind_of?(IPETEdge) }
        # (1) get all referenced outgoing blocks
        out_blocks = {}
        lhs.each { |edge,coeff| out_blocks[edge.source] = 0 }
        # (2) for each block, find minimum coeff for all of its outgoing edges
        #     and replace edges by block
        out_blocks.keys.each { |b|
          edges = b.successors.map { |b2| IPETEdge.new(b,b2,target_level) }
          edges = [ IPETEdge.new(b,:exit,target_level) ] if edges.empty?
          min_coeff = edges.map { |e| lhs[e] }.min
          if min_coeff != 0
            edges.each { |e| lhs[e] -= min_coeff ; lhs.delete(e) if lhs[e] == 0 }
            lhs[b] += min_coeff
          end
        }
      end

      # Create flow-fact
      terms = TermList.new(lhs.map { |v,c| Term.new(v.ref,c) })
      scope = entry[target_level]
      info "Adding transformed constraint #{constr} -> in #{scope} #{terms} #{constr.op} #{constr.rhs}" if options.verbose
      ff = FlowFact.new(scope.ref, terms, constr.op, constr.rhs)
      new_flowfacts.push(ff)
    end

    new_flowfacts
  end
end

end # module PML
