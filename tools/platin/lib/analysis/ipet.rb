#
# platin tool set
#
# ILP/IPET module
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
  def var_indices
    @lhs.keys
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
  attr_reader :variables, :constraints, :costs, :options, :vartype, :solvertime, :elim_steps
  # variables ... array of distinct, comparable items
  def initialize(options = nil)
    @solvertime = 0
    @options = options
    @variables = []
    @indexmap = {}
    @vartype = {}
    @eliminated = Hash.new(false)
    @constraints = Set.new
    @elim_steps = 0
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
  end
  # add a new variable
  def add_variable(v, vartype=:dst)
    raise Exception.new("Duplicate variable: #{v}") if @indexmap[v]
    @variables.push(v)
    index = @variables.length # starting with 1
    @indexmap[v] = index
    @vartype[v] = vartype
    @eliminated.delete(v)
    add_indexed_constraint({index => -1},"less-equal",0,"__lower_bound_v#{index}",Set.new([:positive]))
    index
  end
  # add constraint:
  # terms_lhs .. [ [v,c] ]
  # op        .. "equal" or "less-equal"
  # const_rhs .. integer
  def add_constraint(terms_lhs,op,const_rhs,name,tag)
    terms_indexed = Hash.new(0)
    terms_lhs.each { |v,c|
      terms_indexed[index(v)] += c
    }
    add_indexed_constraint(terms_indexed,op,const_rhs,name,Set.new([tag]))
  end

  # conceptually private; friend VariableElimination needs access
  def delete_varindex(var_index)
    @eliminated[var_index] = true
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

#
# IPETEdges are either:
# - edges beween CFG blocks
# - call edges
# - edges between relation graph nodes
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
  def cfg_edge?
    return false unless source.kind_of?(Block)
    return false unless :exit == target || target.kind_of?(Block)
    true
  end
  def cfg_edge
    assert("IPETEdge#cfg_edge: not a edge between blocks") { cfg_edge? }
    (:exit == target) ? source.edge_to_exit : source.edge_to(target)
  end
  def call_edge?
    source.kind_of?(Instruction) || target.kind_of?(Function)
  end
  def relation_graph_edge?
    source.kind_of?(RelationNode) || target.kind_of?(RelationNode)
  end
  def to_s
    arrow  = @level == :src ? "~>" : "->"
    "#{@source}#{arrow}#{:exit == @target ? 'exit' : @target}"
  end
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
    ipet.add_constraint(lhs, "equal", 0, "callsite_#{callsite.qname}",:callsite)

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
    ipet.add_constraint(lhs,"less-equal",0,"calledges_#{callsite.qname}",:callsite)

    # return call edges
    calledges
  end

  # frequency of analysis entry is 1
  def add_entry_constraint(entry_function)
    ipet.add_constraint(function_frequency(entry_function),"equal",1,"structural_entry",:structural)
  end

  # frequency of function is equal to sum of all callsite frequencies
  def add_function_constraint(function, calledges)
    lhs = calledges.map { |e| [e,-1] }
    lhs.concat(function_frequency(function,1))
    ipet.add_constraint(lhs,"equal",0,"callers_#{function}",:callsite)
  end

  # frequency of incoming is frequency of outgoing edges
  def add_block_constraint(block)
    return if block.predecessors.empty?
    lhs = sum_incoming(block,-1) + sum_outgoing(block)
    lhs.push [IPETEdge.new(block,:exit,level),1] if block.may_return?
    ipet.add_constraint(lhs,"equal",0,"structural_#{block.qname}",:structural)
  end

  # frequency of incoming edges is frequency of block
  def add_block_variable_constraint(block)
    lhs = block_frequency(block) + [[block, -1]]
    ipet.add_constraint(lhs,"equal",0,"block_#{block.qname}", :structural)
  end

  # frequency of incoming is frequency of outgoing edges is 0
  def add_infeasible_block(block)
    add_block_constraint(block)
    unless block.predecessors.empty?
      ipet.add_constraint(sum_incoming(block),"equal",0,"structural_#{block.qname}_0in",:infeasible)
    end
    unless block.successors.empty?
      ipet.add_constraint(sum_outgoing(block),"equal",0,"structural_#{block.qname}_0out",:infeasible)
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
    [[IPETEdge.new(edgeref.source, edgeref.target ? edgeref.target : :exit, level), factor ]]
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
    function.blocks.each_with_index do |bb,ix|
      next if ix != 0 && bb.predecessors.empty? # data block
      bb.successors.each do |bb2|
        yield IPETEdge.new(bb,bb2,level)
      end
      if bb.may_return?
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
  def build(entry, opts = { :mbb_variables =>  false })
    mf_functions = reachable_set(entry[:dst]) do |mf_function|
      succs = Set.new
      mf_function.callsites.each { |cs|
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
      f.blocks.each_with_index do |block,ix|
        next if block.predecessors.empty? && ix != 0 # exclude data blocks (for e.g. ARM)
        if @mc_model.infeasible.include?(block)
          @mc_model.add_infeasible_block(block)
          next
        end
        @mc_model.add_block_constraint(block)
        if opts[:mbb_variables]
          @ilp.add_variable(block)
          @mc_model.add_block_variable_constraint(block)
        end
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
      if scope && scope.reference.kind_of?(FunctionRef) && scope.function == entry[model]
        add_calltargets(cs.instruction, targets.map { |t| t.function} , model)
      end
      # set infeasible blocks
      scope,bref = ff.get_block_infeasible
      if scope && scope.reference.kind_of?(FunctionRef) && scope.function == entry[model]
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
  def add_flowfact(ff, tag = :flowfact)
    model = ff.level == "machinecode" ? @mc_model : @bc_model
    raise Exception.new("IPETBuilder#add_flowfact: cannot add bitcode flowfact without using relation graph") unless model
    lhs = []
    ff.lhs.each { |term|
      unless term.context.empty?
        warn("IPETBuilder#add_flowfact: context sensitive program points not supported: #{ff}")
      end
      if term.ppref.kind_of?(FunctionRef)
        lhs += model.function_frequency(term.ppref.function, term.factor)
      elsif term.ppref.kind_of?(BlockRef)
        lhs += model.block_frequency(term.ppref.block, term.factor)
      elsif term.ppref.kind_of?(EdgeRef)
        lhs += model.edgeref_frequency(term.ppref, term.factor)
      elsif term.ppref.kind_of?(InstructionRef)
        # XXX: exclusively used in refinement for now
        return false
      else
        raise Exception.new("IPETBuilder#add_flowfact: Unknown reference type: #{term.ppref.class}")
      end
    }
    scope = ff.scope
    unless scope.context.empty?
      warn("IPETBUilder#add_flowfact: context sensitive scopes not supported: #{ff}")
      return false
    end
    if scope.reference.kind_of?(FunctionRef)
      lhs += model.function_frequency(scope.reference.function, -ff.rhs)
    elsif scope.reference.kind_of?(LoopRef)
      lhs += model.sum_loop_entry(scope.reference.loopblock, -ff.rhs)
    else
      raise Exception.new("IPETBuilder#add_flowfact: Unknown scope type: #{scope.reference.class}")
    end
    begin
      name = "ff_#{@ffcount+=1}"
      ilp.add_constraint(lhs, ff.op, 0, name, tag)
      name
    rescue UnknownVariableException => detail
      debug(@options,:ipet) { "Skipping constraint: #{detail}" }
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
    # Our LCTES 2013 paper describes 5 sets of constraints referenced below
    # map from src/dst edge to set of corresponding relation edges (constraint set (3) and (4))
    rg_edges_of_edge   = { :src => {}, :dst => {} }
    # map from progress node to set of outgoing src/dst edges (constraint set (5))
    rg_progress_edges = { }
    each_relation_edge(rg) do |edge|
      level = edge.level
      source_block = edge.source.get_block(level)
      target_block = (edge.target.type == :exit) ? :exit : (edge.target.get_block(level))

      assert("Bad RG: #{edge}") { source_block && target_block }
      # (3),(4)
      (rg_edges_of_edge[level][IPETEdge.new(source_block,target_block,level)] ||=[]).push(edge)
      # (5)
      if edge.source.type == :entry || edge.source.type == :progress
        rg_progress_edges[edge.source] ||= { :src => [], :dst => [] }
        rg_progress_edges[edge.source][level].push(edge)
      end
    end
    # (3),(4)
    rg_edges_of_edge.each do |level,edgemap|
      edgemap.each do |edge,rg_edges|
        lhs = rg_edges.map { |rge| [rge,1] } + [[edge,-1]]
        @ilp.add_constraint(lhs, "equal", 0, "rg_edge_#{edge.qname}", :structural)
      end
    end
    # (5)
    rg_progress_edges.each do |progress_node, edges|
      lhs = edges[:src].map { |e| [e,1] } + edges[:dst].map { |e| [e,-1] }
      @ilp.add_constraint(lhs, "equal", 0, "rg_progress_#{progress_node.qname}", :structural)
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

end # module PML
