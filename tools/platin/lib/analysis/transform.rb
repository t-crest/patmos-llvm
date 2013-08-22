#
# platin tool set
#
# Fourier-Motzkin Elimination and Flow-Fact Transformation
#
require 'core/utils'
require 'core/pml'
require 'analysis/ipet'
require 'set'
module PML

# constraint reference for FM elimination
#
class ConstraintRef
  # id, referenced constraint, variables to eliminate
  attr_reader :cid, :constraint, :elim_vars

  # number of variables to eliminate; used as measure for equation selection
  attr_reader :num_elim_vars

  # Status is one out of :active, :garbage
  attr_accessor :status
  def initialize(cid, constraint, all_elim_vars)
    @cid, @constraint = cid, constraint
    @num_elim_vars = 0
    @elim_vars = Set.new
    @constraint.lhs.keys.each { |v|
      @elim_vars.add(v) if all_elim_vars.include?(v)
    }
    @num_elim_vars = @elim_vars.size
    @elim_vars.freeze
    @status = :active
  end

  def num_vars
    constraint.lhs.size
  end

  def to_s
    assert("num_elim_vars inconsistent") { @num_elim_vars == @elim_vars.size }
    "CR#<cid=#{cid},num_vars=#{num_vars},elim_vars=#{@elim_vars.to_a},constr=#{@constraint},status=#{status}>"
  end
  def ==(other)
    @cid == other.cid
  end
  def eql?(other)
    @cid == other.cid
  end
  def hash
    cid
  end
  def <=>(other)
    c = num_elim_vars <=> other.num_elim_vars
    return c unless c == 0
    @cid <=> other.cid
  end
end


# set of constraint refs ordered by elim_vars size
# efficient drop-in replacement for SortedSet
#
class RefSet
  def initialize
    @store = {}
  end
  def add(cref)
    sz = cref.num_elim_vars
    list = (@store[sz] ||= Array.new)
    list.push(cref)
    @minsz = sz if ! @minsz || sz < @minsz
  end
  def empty?
    @minsz.nil?
  end
  def pop
    return nil if empty?
    list = @store[@minsz]
    v = list.pop
    assert("RefSet#pop: wrong bucket") { @minsz == v.num_elim_vars }
    if list.empty?
      @store.delete(@minsz)
      @minsz = @store.keys.min
    end
    v
  end
  def dump(io=$stdout)
    io.puts "RefSet"
    @store.each { |sz,vs|
      io.puts " #{sz} variables to eliminate:"
      vs.each { |v|
        io.puts " - #{v}"
      }
    }
  end
end

class VariableElimination
  attr_reader :elim_steps

  def initialize(ilp)
    @ilp = ilp
    @elim_steps = 0
  end

  #
  # eliminate set of variables (which must have no cost assigned in the ILP)
  #
  def eliminate_set(vars)

    # set of variable ids left to eliminate
    elim_vids = Set.new
    # map of known constraints to constraint references
    known_constraints = {}
    # set of unaffected constraints
    unaffected = Set.new
    # map from variable index to equalities/inequalities it is involved in
    eq_constraints, bound_constraints = {}, {}
    # equations that hold a variable to eliminate
    elim_eqs = RefSet.new

    # initialize variable->constraint dictionaries
    @ilp.variable_indices.each { |vid|
      eq_constraints[vid] = Set.new
      bound_constraints[vid] = Set.new
    }

    # initialize set of variables to eliminate
    vars.each { |v|
      raise Exception.new("VariableElimination: variable #{v} has cost assigned and cannot be eliminated") if @ilp.get_cost(v) > 0
      elim_vids.add(@ilp.index(v))
    }

    # setup initial constraints
    @ilp.constraints.each { |constr|
      cref = ConstraintRef.new(known_constraints.size, constr, elim_vids)
      known_constraints[constr] = cref
      if constr.lhs.all? { |v,c| ! elim_vids.include?(v) }
        unaffected.add(cref)
      else
        dict = (constr.op == "equal") ? eq_constraints : bound_constraints
        elimeq_list = []
        constr.lhs.each { |vid,c|
          dict[vid].add(cref)
        }
        elim_eqs.add(cref) if constr.op == "equal" && cref.num_elim_vars > 0
      end
    }

    # eliminate all variables
    while ! elim_vids.empty?
      elimvar, elimeq = nil, nil
      while ! elim_eqs.empty?
        tmpeq = elim_eqs.pop
        if tmpeq.status == :active
          elimeq = tmpeq
          elimvar = elimeq.elim_vars.first
          break
        end
      end
      if ! elimvar
        elimvar = elim_vids.first
      end

      # "Eliminating #{var_by_index(elimvar)}: #{eq_constraints[elimvar].size}/#{bound_constraints[elimvar].size}"

      if elimeq # Substitution
        # mark equation as garbage and remove it from all eq_constraints
        elimeq.status = :garbage
        elimeq.constraint.var_indices.each { |v|
          tmp = eq_constraints[v].delete(elimeq)
          raise Exception.new("Internal Error: inconstinstent eq_constraints dictionary") unless tmp
        }

        # substitute in all constraints referenced by elimvar
        [eq_constraints,bound_constraints].each { |dict|
          dict[elimvar].each { |subst_constr|
            # mark old constraint as garbage and remove it from dict
            subst_constr.status = :garbage
            subst_constr.constraint.var_indices.each { |v|
              tmp = dict[v].delete(subst_constr)
              raise Exception.new("Internal Error: inconsistent constraint dictionary") unless tmp
            }

            # substitute
            @elim_steps += 1
            neweq = constraint_substitution(elimvar, elimeq.constraint, subst_constr.constraint)

            # create new constraint reference, if necessary
            if neweq && ! known_constraints[neweq]
              cref = known_constraints[neweq] = ConstraintRef.new(known_constraints.size, neweq, elim_vids)
              cref.constraint.var_indices.each { |v|
                dict[v].add(cref)
              }
              elim_eqs.add(cref) if neweq.op == "equal" && cref.num_elim_vars > 0
            end
          }
        }
      else # FM elimination
        raise Exception.new("Internal error: equations left") unless eq_constraints[elimvar].empty?
        l, u = [], []
        bound_constraints[elimvar].each { |cref|
          coeff = cref.constraint.get_coeff(elimvar)
          if coeff < 0
            l.push(cref)
          else
            u.push(cref)
          end
          cref.status = :garbage
          # remove from all bound_constraints
          cref.constraint.var_indices.each { |v|
            tmp = bound_constraints[v].delete(cref)
            raise Exception.new("Internal Error: inconstinstent bound_constraints dictionary") unless tmp
          }
        }
        l.each do |l_constr|
          u.each do |u_constr|
            @elim_steps += 1
            newconstr = transitive_constraint(elimvar, l_constr.constraint, u_constr.constraint)
            if newconstr && ! known_constraints[newconstr]
              cref = known_constraints[newconstr] = ConstraintRef.new(known_constraints.size, newconstr, elim_vids)
              cref.constraint.var_indices.each { |v|
                bound_constraints[v].add(cref)
              }
            end
          end
        end
      end
      @ilp.delete_varindex(elimvar)
      elim_vids.delete(elimvar)
    end

    @ilp.reset_constraints
    known_constraints.values.each { |cref|
      if cref.status != :garbage
        raise Exception.new("Internal Error: no constraint") unless cref.constraint
        @ilp.constraints.add(cref.constraint)
      end
    }
  end

  # Given constraints
  #  (A)   e_coeff e_var + e_rterms = e_rhs               | e_constr.lhs = e_coeff e_var + e_rtterms
  #  (B)   c_coeff * e_var + c_rterms <=> c_rhs (constr)  | c_constr.lhs = c_coeff e_var + c_rterms
  # NB: As we must not multiply (B) by a negative number (in case of an inequality), we implicitly negate
  # (A) if e_coeff is negative by multiplying e_coeff, e_rterms and e_rhs by e_coeff.signum
  # multiply constraints by c_coeff and e_coeff, respectively
  #  (A')  c_coeff e_coeff e_var + c_coeff e_rterms = c_ceoff e_rhs
  #  (B')  e_coeff c_coeff e_var + e_coeff c_rterms <=> e_coeff c_rhs
  # and substitute
  #  (B'') e_coeff c_term - c_coeff e_rterms <=> e_coeff c_rhs - c_ceoff e_rhs
  #
  def constraint_substitution(e_var, e_constr, c_constr)
    signed_e_coeff = e_constr.get_coeff(e_var)
    e_sign  = signed_e_coeff <=> 0
    e_coeff = signed_e_coeff * e_sign
    e_rhs   = e_constr.rhs * e_sign
    c_coeff = c_constr.get_coeff(e_var)
    terms = c_constr.lhs

    c_rterms = terms.merge(terms) { |v,c| e_coeff * c }               # multiply by e_coeff
    e_constr.lhs.each { |v,c| c_rterms[v] -= c_coeff * (e_sign * c) } # subtract c_coeff * e_terms
    c_rhs = c_constr.rhs * e_coeff - e_rhs * c_coeff                  # substract e_rhs * e_coeff

    raise Exception.new("Internal error in constraint_substitution: #{e_var},#{e_constr},#{c_constr}") if c_rterms[e_var] != 0

    @ilp.create_indexed_constraint(c_rterms, c_constr.op, c_rhs, c_constr.name, e_constr.tags + c_constr.tags)
  end

  # Given constraints
  #  (A) l_coeff e_var + l_rterms <= l_rhs  | l_coeff < 0
  #  (B) u_coeff e_var + u_rterms <= u_rhs  | u_coeff > 0
  # multiply constraints by u_coeff and - l_coeff, respectively
  #  (A') u_coeff l_coeff e_var  + u_coeff l_rterms <= u_coeff l_rhs
  #  (B') -l_coeff u_coeff e_var + -l_coeff u_rterms <= -l_coeff u_rhs
  # and apply transitivty lemma to get
  #  (C)  u_coeff l_rterms - l_coeff u_rterms <= u_coeff l_rhs - l_coeff u_rhs
  #
  def transitive_constraint(e_var, l_constr, u_constr)
    terms = Hash.new(0)
    l_coeff = l_constr.get_coeff(e_var)
    u_coeff = u_constr.get_coeff(e_var)
    assert("Not a lower bound for #{e_var}: #{l_constr.inspect}") { l_coeff < 0 }
    assert("Not an upper bound for #{e_var}: #{u_constr.inspect}") unless u_coeff > 0

    l_constr.lhs.each { |v,c|
      terms[v] += u_coeff * c
    }
    u_constr.lhs.each { |v,c|
      terms[v] -= l_coeff * c
    }
    rhs = u_coeff * l_constr.rhs - l_coeff * u_constr.rhs

    assert("Variable #{e_var} not eliminated as it should be") { terms[e_var] == 0 }
    @ilp.create_indexed_constraint(terms, l_constr.op, rhs, l_constr.name+"<>"+u_constr.name, l_constr.tags + u_constr.tags)
  end

end

class FlowFactTransformation

  attr_reader :pml, :options

  def initialize(pml,options)
    @pml, @options = pml, options
  end

  # Copy flowfacts
  def copy(flowfacts)
    copied = pml.flowfacts.add_copies(flowfacts, options.flow_fact_output)
    statistics("IPET","Flowfacts copied (=>#{options.flow_fact_output})" => copied.length) if options.stats
  end

  # Simplify
  def simplify(machine_entry, flowfacts)
    builder_opts = { :use_rg => false }
    if options.transform_eliminate_edges
      builder_opts[:mbb_variables] = true
    else
      warn("TransformTool#simplify: no simplifications enabled")
    end

    # Filter flow facts that need to be simplified
    copy, simplify = [], []
    flowfacts.each { |ff|
      if options.transform_eliminate_edges && ! ff.get_calltargets && ff.references_edges?
        simplify.push(ff)
      elsif options.transform_eliminate_edges && ff.references_empty_block?
        simplify.push(ff)
      else
        copy.push(ff)
      end
    }
    copied = pml.flowfacts.add_copies(copy, options.flow_fact_output)

    # Build ILP for transformation
    entry = { :dst => machine_entry, :src => pml.bitcode_functions.by_name(machine_entry.label) }
    ipet = build_model(entry, copied, builder_opts)
    simplify.each { |ff| ipet.add_flowfact(ff, :simplify) }

    # Elimination
    ilp = ipet.ilp
    constraints_before = ilp.constraints.length
    elim_set = []
    ilp.variables.each do |var|
      if var.kind_of?(Instruction)
        debug(options,:ipet) { "Eliminating Instruction: #{var}" }
        elim_set.push(var)
      elsif options.transform_eliminate_edges && var.kind_of?(IPETEdge) && var.cfg_edge?
        debug(options,:ipet) { "Eliminating IPET Edge: #{var}" }
        elim_set.push(var)
      elsif options.transform_eliminate_edges && var.kind_of?(Block) && var.instructions.empty?
        debug(options,:ipet) { "Eliminating empty block: #{var}" }
        elim_set.push(var)
       end
    end
    VariableElimination.new(ilp).eliminate_set(elim_set)

    # Extract and add new flow facts
    new_ffs = extract_flowfacts(ilp, entry, :dst, [:simplify])
    new_ffs.each { |ff| pml.flowfacts.add(ff) }
    statistics("TRANSFORM",
               "Constraints after 'simplify' FM-elimination (#{constraints_before} =>)" =>
               ilp.constraints.length,
               "Unsimplified flowfacts copied (=>#{options.flow_fact_output})" =>
               copied.length,
               "Simplified flowfacts (#{options.flow_fact_srcs} => #{options.flow_fact_output})" =>
               new_ffs.length) if options.stats
  end

  def transform(machine_entry, flowfacts, target_level)
    # Build ILP for transformation
    entry = { :dst => machine_entry, :src => pml.bitcode_functions.by_name(machine_entry.label) }
    ilp = build_model(entry, flowfacts, :use_rg => true).ilp

    # If direction up/down, eliminate all vars but dst/src
    info "Running transformer to level #{target_level}" if options.verbose
    constraints_before = ilp.constraints.length
    elim_set = ilp.variables.select { |var|
      ilp.vartype[var] != target_level || ! var.kind_of?(IPETEdge) || ! var.cfg_edge?
    }
    VariableElimination.new(ilp).eliminate_set(elim_set)

    # Extract and add new flow facts
    new_ffs = extract_flowfacts(ilp, entry, target_level, [:flowfact, :callsite])
    new_ffs.each { |ff| pml.flowfacts.add(ff) }
    statistics("TRANSFORM",
               "Constraints after FM-elimination (#{constraints_before} =>)" => ilp.constraints.length,
               "transformed flowfacts (#{options.flow_fact_srcs} => #{options.flow_fact_output})" => new_ffs.length,
               "elimination steps" => ilp.elim_steps) if options.stats
  end

private

  #
  # entry      ... { :dst => <machine-function , :src => <bitcode-function> }
  # flowfacts  ... [ <FlowFact f> ]
  # opts  :use_rg          => <boolean> ... whether to enable relation graphs
  #       :mbb_variables => <boolean> ... add variables representing basic blocks
  #
  def build_model(entry, flowfacts, opts = { :use_rg => false })
    # ILP for transformation
    ilp  = ILP.new(@options)

    # IPET builder
    builder_opts = options.dup
    builder_opts.use_relation_graph = opts[:use_rg]
    ipet = IPETBuilder.new(pml,builder_opts,ilp)

    # Build IPET (no cost) and add flow facts
    ipet.build(entry, :mbb_variables => true) { |edge| 0 }
    flowfacts.each { |ff| ipet.add_flowfact(ff) }
    ipet.refine(entry, flowfacts)
    ipet
  end

  def extract_flowfacts(ilp, entry, target_level, tags = [:flowfact, :callsite])
    new_flowfacts = []
    attrs = { 'origin' =>  options.flow_fact_output,
              'level'  => (target_level == :src) ? "bitcode" : "machinecode" }
    ilp.constraints.each do |constr|
      lhs = constr.named_lhs
      name = constr.name

      # Constraint is boring if it was derived from positivity and structural constraints only
      interesting = constr.tags.any? { |tag| tags.include?(tag) }
      next unless interesting

      # Simplify: edges->block if possible (lossless; see eliminate_edges for potentially lossy transformation)
      unless lhs.any? { |var,_| ! var.kind_of?(IPETEdge) }
        # (1) get all referenced outgoing blocks
        out_blocks = {}
        lhs.each { |edge,coeff| out_blocks[edge.source] = 0 }
        # (2) for each block, find minimum coeff for all of its outgoing edges
        #     and replace edges by block
        out_blocks.keys.each { |b|
          edges = b.successors.map { |b2| IPETEdge.new(b,b2,target_level) }
          edges = [ IPETEdge.new(b,:exit,target_level) ] if b.may_return?
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
      debug(options,:ipet) {
        "Adding transformed constraint #{name} #{constr.tags.to_a}: #{constr} -> in #{scope} :" +
        "#{terms} #{constr.op} #{constr.rhs}"
      }
      ff = FlowFact.new(scope.ref, terms, constr.op, constr.rhs, attrs.dup)
      new_flowfacts.push(ff)
    end
    new_flowfacts
  end
end

end # module PML
