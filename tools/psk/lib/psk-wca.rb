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
    @options = options || OpenStruct.new(:debug=>false)
    @variables = []
    @indexmap = {}
    vars.each { |v| add_variable(v) }
    reset_cost
    @constraints = []
  end
  def add_variable(v)
    puts "Adding variable #{v}" if @options.verbose
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
    #puts "Adding cost #{variable} -> #{cost}"
    @costs[variable] += cost
  end

  # add constraint:
  # lhs .. [ [v,c] ]
  # op  .. "equal" or "less-equal"
  # rhs .. integer
  def add_constraint(lhs,op,rhs,name=nil)
    name = "__#{@constraints.size}" unless name
    puts "Adding constraint #{lhs} #{op} #{rhs} (#{name})" if @options.verbose
    lhs_indexed = Hash.new(0)
    lhs.each { |v,c| 
      lhs_indexed[index(v)] += c
    }
    @constraints.push([name,lhs_indexed.to_a,lpsolve_op(op),rhs])
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
    lp.print_lp if @options.debug
    lp.set_verbose(0)
    r = lp.solve
    # read solution
    lp.print_solution(-1) if @options.debug
    obj = lp.objective
    raise lp_solve_error(r) unless r == 0
    if (obj-obj.round.to_f).abs > EPS
      raise Exception.new("Untolerable floating point inaccuracy > #{EPS} in objective #{obj}")
    end
    freqmap = extract_frequencies(lp.get_variables)
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
      internal_error ("Unsupported comparison operator #{op}")
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
  def initialize(bb_source, bb_target)
    @source,@target = bb_source, bb_target
    @qname = "#{@source.qname}->#{@target.qname}"
  end
  def backedge?
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
  def add_entry_constraint(entry_function)
    lhs = sum_outgoing(entry_function.blocks.first)
    ipet.add_constraint(lhs,"equal",1,"entry")
  end
  def add_block_constraint(block)
    return if (block['predecessors']||[]).empty?
    lhs = if(block['successors']||[]).empty?
            lhs = sum_incoming(block,-1) + [[block,1]]
          else
            lhs = sum_incoming(block,-1) + sum_outgoing(block)
          end
    ipet.add_constraint(lhs,"equal",0,"structural_#{block.qname}")
  end
  def add_call_constraint(f,css)
    lhs = sum_outgoing(f.blocks.first)
    css.each do |cs|
      lhs.concat(block_frequency(cs.block, -1))
    end
    ipet.add_constraint(lhs,"equal",0,"call_#{f.qname}")    
  end
  def block_frequency(block, factor=1)
    if(block['successors']||[]).empty? # return block variable
      [[block,factor]]
    else
      sum_outgoing(block,factor)
    end
  end
  def sum_incoming(block, factor=1)
    block['predecessors'].map { |bname|
      pred=block.function.blocks.get(bname) # XXX: ugly
      [Edge.new(pred,block), factor]
    }
  end
  def sum_outgoing(block, factor=1)
    block['successors'].map { |bname|
      succ=block.function.blocks.get(bname) # XXX: ugly
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
      (bb['successors']||[]).each do |bname|
        bb2 = function.blocks.get(bname) # XXX: ugly
        yield  Edge.new(bb,bb2)
      end
      if ! bb['successors'] || bb['successors'].empty?
        yield bb
      end
    end
  end
# end of class IPETBuilder
end

class MachineCodeIPETBuilder < IPETBuilder

  def initialize(pml)
    @pml = pml
    @ipet = LpSolveIPET.new([])
  end

  # Build basic IPET structure.
  # Yields blocks, so the caller can compute their cost
  def build(entry)    
    @callsites = {}
    functions = reachable_set(entry) { |f|
      cssuccs = f.callgraph_successors do |callee_name|
        @pml.machine_functions.originated_from(callee_name)
      end
      cssuccs.each { |f2,cs| (@callsites[f2]||=[]).concat(cs) }
      cssuccs.keys
    }
    functions.each do |f|
      each_edge(f) do |edge|
        @ipet.add_variable(edge)
        if edge.kind_of?(Block)
          cost = yield edge
          @ipet.add_cost(edge, cost)
        else
          cost = yield edge.source
          @ipet.add_cost(edge, cost)
        end
      end
      f.blocks.each do |block|
        add_block_constraint(block)
      end
    end
    add_entry_constraint(entry)
    @callsites.each do |f,css|
      add_call_constraint(f,css)
    end
  end

  # currently we only support block frequency bounds
  def add_flowfact(ff)
    scope, bref, freq = ff.get_block_frequency_bound
    # XXX: ugly => we should have a scope object here    
    scope_function = @pml.machine_functions.get(scope['function'])
    block_function = @pml.machine_functions.get(bref['function'])    
    block = block_function.blocks.get(bref['block'])
    # XXX: ugly again
    lhs = []
    if scope.keys == ['function']
      lhs = block_frequency(block) + sum_outgoing(scope_function.blocks.first, -freq)
    elsif scope.keys == ['function','loop']
      loop = scope_function.blocks.get(scope['loop'])
      lhs  = block_frequency(block) + sum_loop_entry(loop, -freq)
    else
      return false
    end
    ipet.add_constraint(lhs,"less-equal", 0, nil)
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
    builder = MachineCodeIPETBuilder.new(pml)
    entry = pml.machine_functions.originated_from(options.entry)
    builder.build(entry) do |block|
      block.instructions.length # pseudo cost
    end
    pml.flowfacts.each do |ff|
      builder.add_flowfact(ff)
    end

    cycles,freqs = builder.ipet.solve_max
    puts [cycles,freqs].inspect if options.verbose

    entry = TimingEntry.new(entry.ref, cycles, 'level' => 'machinecode', 'origin' => 'psk')
    pml.add_timing(entry)
    pml
  end
  def WcaTool.add_options(opts,options)
    opts.on("-e", "--entry FUNCTION", "Name of the function to analyse") { |f| options.entry = f }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Calculate WCET using lp_solve and a simple timing model
EOF
  options, args = PML::optparse(0, "", SYNOPSIS, :type => :io) do |opts,options|
    WcaTool.add_options(opts,options)
  end
  WcaTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
