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
    # $dbgs.puts "Adding variable #{v}" if @options.debug
    @variables.push(v)
    @indexmap[v] = @variables.length # starting with 1
  end
  # index of a variable
  def index(variable)
    @indexmap[variable] or raise Exception.new("unknown variable: #{variable}")
  end

  # set cost of all variables to 0
  def reset_cost
    @costs = Hash.new(0)
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
    @constraints.push([name,terms_indexed.to_a,lpsolve_op(op),const_rhs])
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
    }.join(" = ")
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
    #raise lp_solve_error(r) unless r == 0
    #if (obj-obj.round.to_f).abs > EPS
    #  raise Exception.new("Untolerable floating point inaccuracy > #{EPS} in objective #{obj}")
    #end    
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

class Edge
  attr_reader :qname,:source,:target
  def initialize(edge_source, edge_target)
    @source,@target = edge_source, edge_target
    @qname = "#{@source.qname}->#{@target.qname}"
  end
  def backedge?
    return false unless source.kind_of?(Block)
    return false unless source.loopnest >= target.loopnest && target.loopheader?
    source['loops'][-1] == target.name # ugly
  end
  def to_s ; @qname ; end
  def hash;  @qname.hash ; end
  def ==(other); qname == other.qname ; end
  def eql?(other); self == other; end
end

class IPETBuilder
  attr_reader :pml, :ipet
  def initialize(ipet)
    @ipet = ipet
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
      calledge = Edge.new(callsite, f)
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
    return if (block.predecessors == block.successors)
    lhs = if block.successors.empty?
            lhs = sum_incoming(block,-1) + [[block,1]]
          else
            lhs = sum_incoming(block,-1) + sum_outgoing(block)
          end
    ipet.add_constraint(lhs,"equal",0,"structural_#{block.qname}")
  end

  def function_frequency(function, factor = 1)
    block_frequency(function.blocks.first, factor)
  end
  def block_frequency(block, factor=1)
    if block.successors.empty? # return block variable
      [[block,factor]]
    else
      sum_outgoing(block,factor)
    end
  end
  def sum_incoming(block, factor=1)
    block.predecessors.map { |pred|
      [Edge.new(pred,block), factor]
    }
  end
  def sum_outgoing(block, factor=1)
    block.successors.map { |succ|
      [Edge.new(block,succ), factor]
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
        yield Edge.new(bb,bb2)
      end
      if bb.successors.empty? # returns
        yield bb
      end
    end
  end
# end of class IPETBuilder
end

class MachineCodeIPETBuilder < IPETBuilder

  def initialize(pml, options)
    super(LpSolveIPET.new([], options))
    @ffcount = 0
    @pml, @options = pml, options
    @calltargets = {}
    @infeasible = Set.new
  end

  def add_calltargets(cs, fs)
    @calltargets[cs] = fs
  end

  def set_infeasible(block)
    @infeasible.add(block)
  end

  # Build basic IPET structure.
  # Yields blocks, so the caller can compute their cost
  def build(entry)
    functions = reachable_set(entry) do |function|
      succs = Set.new
      function.each_callsite { |cs|
        next if @infeasible.include?(cs.block)
        cs['callees'].each { |fname|
          if(fname == '__any__')
            # external flow fact
            @calltargets[cs].each { |f| succs.add(f) }
          else
            # compiler information
            f = @pml.machine_functions.by_label(fname)
            add_calltargets(cs, [f])
            succs.add(f)
          end
        }
      }
      each_edge(function) do |edge|
        ipet.add_variable(edge)
        cost = yield edge
        ipet.add_cost(edge, cost)
      end
      succs # return successors to reachable_set
    end
    function_callers = {}
    functions.each do |f|
      f.blocks.each do |block|
        add_block_constraint(block)
        next if @infeasible.include?(block)
        # Good idea for debugging, as it makes it easy to spot unbounded blocks
        ipet.add_constraint( block_frequency(block), "less-equal", 1000000)#  if @options.debug
        block.callsites.each do |cs|
          call_edges = add_callsite(cs, @calltargets[cs])
          call_edges.each do |ce|
            (function_callers[ce.target] ||= []).push(ce)
          end
        end
      end
    end
    add_entry_constraint(entry)
    function_callers.each do |f,ces|
      add_function_constraint(f, ces)
    end
  end

  # currently we only support block frequency bounds
  def add_flowfact(ff)
    scope, bref, freq = ff.get_block_frequency_bound
    lhs = []
    if scope.kind_of?(FunctionRef)
      lhs = block_frequency(bref.block) + function_frequency(scope.function, -freq)
    elsif scope.kind_of?(LoopRef)
      lhs  = block_frequency(bref.block) + sum_loop_entry(scope.loopblock, -freq)
    else
      return false
    end
    begin
      ipet.add_constraint(lhs,"less-equal", 0, "#{ff['classification']}_#{@ffcount+=1}")
    rescue Exception => detail
      #puts "Skipping constraint: #{detail}"
    end
  end
end

# TODO: relation graph strategy
# * Build Callgraph (either IR or machine code)
# * add cost on MC level
# * add structural constraints on MC and IR level
# * add constraints on IR level
# * add relation constraints

class WcaTool
  def WcaTool.run(pml,options)
    options.entry = "main" unless options.entry
    builder = MachineCodeIPETBuilder.new(pml, options)
    entry = pml.machine_functions.by_label(options.analysis_entry)
    pml.flowfacts.each do |ff|
      # set indirect call targets
      scope,cs,targets = ff.get_calltargets
      if scope && scope.kind_of?(FunctionRef) && scope.function == entry
        builder.add_calltargets(cs.instruction, targets)
      end
      # set infeasible blocks
      scope,bref,frequency = ff.get_block_frequency_bound
      if scope && scope.kind_of?(FunctionRef) && scope.function == entry && frequency == 0
        builder.set_infeasible(bref.block)
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
    pml.flowfacts.each do |ff|
      builder.add_flowfact(ff) if ff['level'] == 'machinecode'
    end
    # solve IPET
    cycles,freqs = builder.ipet.solve_max
    if options.verbose
      puts "Cycles: #{cycles}"
      freqs.each do |v,freq|
        puts "#{v}: #{freq}"
      end
    end
    # report result
    entry = TimingEntry.new(entry.ref, cycles, 'level' => 'machinecode', 'origin' => 'platin')
    pml.add_timing(entry)
    pml
  end
  def WcaTool.add_options(opts)
    opts.analysis_entry
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
