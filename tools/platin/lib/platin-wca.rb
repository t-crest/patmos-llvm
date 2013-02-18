#!/usr/bin/env ruby
#
# PSK tool set
#
# Simple Worst-Case Analysis using IPET
#
# TODO: support control-flow relation graphs
#
require 'utils'
include PML
begin
  require 'rubygems'
  require "lpsolve"
rescue Exception => details
  warn "Failed to load library lpsolve"
  info "  ==> aptitude install liblpsolve55-dev [Debian/Ubuntu]"
  info "  ==> gem1.9.1 install lpsolve --pre"
  die "Failed to load required ruby libraries"
end

class UnknownVariableException < Exception
  def initialize(msg)
    super(msg)
  end
end

# Simple interface to lp_solve for IPET problems
class LpSolveIPET
  # Tolarable floating point error in objective
  EPS=0.0001

  attr_reader :variables

  # variables ... array of distinct, comparable items
  def initialize(vars, options = nil)
    @options = options || OpenStruct.new(:verbose=>false,:debug=>false)
    @variables = []
    @indexmap = {}
    vars.each { |v| add_variable(v) }
    reset_cost
    @constraints = []
  end
  def add_variable(v)
    $dbgs.puts "Adding variable #{v}" if @options.debug
    raise Exception.new("Duplicate variable: #{v}") if @indexmap[v]
    @variables.push(v)
    @indexmap[v] = @variables.length # starting with 1
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
    terms_array = terms_indexed.to_a.reject { |v,c| c == 0 }
    if(terms_array == [])
      return if const_rhs == 0
      return if const_rhs >= 0 && op == "less-equal"
      raise Exception.new("Inconsistent constraint: #{terms_lhs} #{op} #{const_rhs}")
    end
    @constraints.push([name,terms_array,lpsolve_op(op),const_rhs])
  end

  # pretty print constraint (XXX: hackish)
  def show_constraint(terms_lhs,op,const_rhs)
    lhs, rhs = Hash.new(0), Hash.new(0)
    terms_lhs.each { |v,c|
      if c > 0
        lhs[v] += c
      else
        rhs[v] -= c
      end
    }
    [lhs.to_a, rhs.to_a + [[nil,const_rhs]]].map { |ts|
      ts.map { |v,c|
        if !v && c == 0
          nil
        elsif !v
          c
        elsif c == 1
          v
        else
          "#{c} #{v}"
        end
      }.compact.join(" + ")
    }.join(op == "equal" ? " = " : " <= ")
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
    lp.print_lp if @options.lp_debug
    lp.set_verbose(0)
    r = lp.solve
    # read solution
    lp.print_solution(-1) if @options.lp_debug
    obj = lp.objective
    freqmap = extract_frequencies(lp.get_variables)
    # raise lp_solve_error(r) unless r == 0
    if (obj-obj.round.to_f).abs > EPS
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
    @constraints.each do |c|
      lp.add_constraintex(*c)
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
        "Error #{r}"
      end
    raise Exception.new("LPSolver Error: #{msg} (E#{r})")
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
    source.loops[source_loop_index] == target
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
    ipet.add_variable(callsite)
    # frequency of call instruction = frequency of block
    lhs = [ [callsite,1] ] + block_frequency(callsite.block,-1)
    ipet.add_constraint(lhs, "equal", 0, "callsite_#{callsite.qname}")

    # create call edges (callsite -> f) for each called function f
    # the sum of all calledge frequencies is equal to the callsite frequency
    calledges = []
    lhs = [ [callsite, 1] ]
    fs.each do |f|
      calledge = Edge.new(callsite, f, level)
      ipet.add_variable(calledge)
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
  attr_reader :ipet
  def initialize(pml, options)
    @ipet     = LpSolveIPET.new([], options)
    @mc_model = IPETModel.new(@ipet, :dst)
    if options.use_relation_graph
      @bc_model = IPETModel.new(@ipet, :src)
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
        @ipet.add_variable(edge)
        cost = yield edge
        @ipet.add_cost(edge, cost)
      end
      succs # return successors to reachable_set
    end
    function_callers = {}
    functions.each do |f|
      add_bitcode_constraints(f) if @bc_model
      f.blocks.each do |block|
        @mc_model.add_block_constraint(block)
        next if @mc_model.infeasible.include?(block)
        # Good idea for debugging, as it makes it easy to spot unbounded blocks
        @ipet.add_constraint( @mc_model.block_frequency(block), "less-equal", 1000000) #if @options.debug
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
      ipet.add_constraint(lhs,"less-equal", 0, "#{ff['classification']}_#{@ffcount+=1}")
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
      @ipet.add_variable(edge)
    end
    each_relation_edge(rg) do |edge|
      @ipet.add_variable(edge)
    end
  end

  # add constraints for bitcode basic blocks and relation graph
  # (only if relation graph is available)
  def add_bitcode_constraints(machine_function)
    return unless @pml.relation_graphs.has_named?(machine_function.name, :dst)
    rg = @pml.relation_graphs.by_name(machine_function.name, :dst)
    bitcode_function = rg.get_function(:src)
    bitcode_function.blocks.each { |block|
      @ipet.add_constraint( @bc_model.block_frequency(block), "less-equal", 1000000000) #if @options.debug
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
        @ipet.add_constraint(rg_edges.map {|rge|[rge,1]}+[[edge,-1]], "equal", 0)
      end
    end
    rg_edges_by_source.each do |_,edges|
      lhs = edges[:src].map { |e| [e,1] } + edges[:dst].map { |e| [e,-1] }
      @ipet.add_constraint(lhs, "equal", 0)
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

# TODO: relation graph strategy
# * Build Callgraph (either IR or machine code)
# * add cost on MC level
# * add structural constraints on MC and IR level
# * add constraints on IR level
# * add relation constraints

class WcaTool
  # XXX: loop-local should be block-loop
  # XXX: calltargets-global are supported, but used during construction
  SUPPORTED_FLOW_FACT_TYPES=%w{loop-local loop-function loop-global block-local block-function block-global infeasible-global}
  def WcaTool.run(pml,options)
    options.entry = "main" unless options.entry
    builder = IPETBuilder.new(pml, options)
    entry = pml.machine_functions.by_label(options.analysis_entry)
    pml.flowfacts.each do |ff|
      # set indirect call targets
      model = (ff.level == "machinecode") ? :dst : :src
      scope,cs,targets = ff.get_calltargets
      if scope && scope.kind_of?(FunctionRef) && scope.function == entry
        builder.add_calltargets(cs.instruction, targets, model)
      end
      # set infeasible blocks
      scope,bref,frequency = ff.get_block_frequency_bound
      if scope && scope.kind_of?(FunctionRef) && scope.function == entry && frequency == 0
        builder.set_infeasible(bref.block, model)
      end
    end
    # build IPET, including cost
    builder.build(entry) do |edge|
      # pseudo cost (1 cycle per instruction)
      if (edge.kind_of?(Block))
        edge.instructions.length
      else
        src = edge.source
        branch_index = nil
        src.instructions.each_with_index { |ins,ix|
          if btargets = ins['branch-targets'] # XXX ugly
            if btargets.include?(edge.target.name)
              branch_index = ix
            end
          end
        }
        if branch_index
          branch_index + pml.delay_slots + 1
        else
          src.instructions.length
        end
      end
    end
    # add flowfacts
    ff_types = options.flow_fact_types
    ff_types = WcaTool::SUPPORTED_FLOW_FACT_TYPES if ff_types == :supported
    pml.flowfacts.each do |ff|
      # skip if no sources where specified, the level is bitcode, and no relation graph should be used
      next if options.flow_fact_srcs == "all" && ff.level == "bitcode" && ! options.use_relation_graph
      # skip unless the source of the flow fact should be used
      next unless options.flow_fact_srcs == "all" || options.flow_fact_srcs.include?(ff.origin)

      # skip unless that kind of flow fact should be used
      # XXX: sweet flowfacts are not classified yet...
      next unless ff_types.include?(ff.classification) || ! ff.classification
      builder.add_flowfact(ff)
    end
    # solve IPET
    cycles,freqs = builder.ipet.solve_max
    if options.verbose
      puts "Cycles: #{cycles}"
      freqs.map { |v,freq|
        [v,freq * builder.ipet.get_cost(v)]
      }.sort { |a,b| b[1] <=> a[1] }.each { |v,cost|
        puts "#{v}: #{freqs[v]} (#{cost} cyc)"
      }
    end
    # report result
    entry = TimingEntry.new(entry.ref, cycles, 'level' => 'machinecode', 'origin' => options.timing_name || 'platin')
    pml.timing.add(entry)
    pml
  end
  def WcaTool.add_options(opts)
    opts.analysis_entry
    opts.flow_fact_selection
    opts.calculates_wcet
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Calculate WCET using lp_solve and a simple timing model
EOF
  options, args = PML::optparse(0, "", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    WcaTool.add_options(opts)
  end
  WcaTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
