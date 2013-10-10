#
# platin tool set
#
# == Cache Persistence Analysis via Conflict-Free Region Identification
#
# See Article [TODO]

require 'core/pml'
require 'analysis/scopegraph'

module PML

class CacheAnalysis

  def initialize(refinement, pml, options)
    @pml, @options, @refinement = pml, options, refinement
  end

  def analyze(entry_function, ipet_builder)
    scope_graph = ScopeGraph.new(entry_function, @refinement, @pml, @options)
    if mc = @pml.arch.method_cache
      mca = CacheRegionAnalysis.new(MethodCacheAnalysis.new(mc, @pml, @options), @pml, @options)
      mca.extend_ipet(scope_graph, ipet_builder)
    elsif ic = @pml.arch.instruction_cache
      ica = CacheRegionAnalysis.new(InstructionCacheAnalysis.new(ic, @pml, @options), @pml, @options)
      ica.extend_ipet(scope_graph, ipet_builder)
    end
    if sc = @pml.arch.stack_cache
      sca = StackCacheAnalysis.new(sc, @pml, @options)
      sca.analyze(scope_graph)
      sca.extend_ipet(ipet_builder)
    end
    if dc = @pml.arch.data_cache
      warn("Datacache at the moment not supported by platin")
    end
  end
end

# +LoadInstruction+ objects represent the load of a certain memory block
# at a certain program point.
class LoadInstruction
  include QNameObject
  attr_reader :insref, :tag
  def initialize(insref, tag)
    @insref, @tag = insref, tag
    @qname = "#{@tag.qname}@#{insref.qname}"
  end
  def function
    @insref.function
  end
  def to_s
    "#{tag}@#{insref}"
  end
end

# +MemoryEdge+ is a variable to account for the load of a certain memory
# block at a certain program point, attributed to a certain edge of the
# incoming block the program point is contained in
class MemoryEdge
  include QNameObject
  attr_reader :edgeref
  def initialize(edgeref, load_instruction)
    @edgeref, @load_instruction = edgeref, load_instruction
    @qname = "#{edgeref.qname}->#{load_instruction.qname}"
  end
  def function
    @load_instruction.insref.function
  end
  def to_s
    @qname = "#{edgeref}->#{@load_instruction}"
  end
end

#
# Cache Region Analyses proceed in two steps,
# identification of conflict-free scopes (or persistence scopes for
# LRU analysis) and modification of the ipet to account for memory
# transfer costs.
#
class CacheRegionAnalysis
  attr_reader :pml, :options

  # cache tags
  class Tag
    include QNameObject
    def initialize(qname, set)
      @qname = "Tag:#{qname}"
      @set = set
    end
  end

  def get_region_graph(node)
    @region_graphs[node]
  end

  def initialize(cache_properties, pml, options)
    @cache, @pml, @options = cache_properties, pml, options
  end

  def extend_ipet(scopegraph, ipet_builder)
    analyze(scopegraph)
    0.upto(@cache.sets) do |set|
      get_all_tags(scopegraph.root, set).each { |tag,load_instructions|
        load_instructions = load_instructions.to_a
        debug(options,:cache) { "Load instructions for tag: #{tag}: #{load_instructions}" }
        debug(options,:cache) { "Scopes for tag #{tag}: #{@conflict_free_scopes[tag].inspect}" }
        # add a variable for each load instruction
        load_instructions.each { |li|
          ipet_builder.ilp.add_variable(li)
          # load instruction is less equal to instruction frequency
          ipet_builder.mc_model.assert_less_equal({li=>1},{li.insref=>1},"load_ins_#{li}",:cache)

          # add load edges to attribute cost to
          load_edges = []
          li.insref.block.outgoing_edges.each { |edge|
            me = MemoryEdge.new(edge, li)
            ipet_builder.ilp.add_variable(me)
            ipet_builder.ilp.add_cost(me, @cache.load_cost(tag))
            # memory edge frequency is less equal to edge frequency
            ipet_builder.mc_model.assert_less_equal({me=>1},{me.edgeref=>1},"load_edge_#{me}",:cache)
            load_edges.push(me)
          }
          # sum of load edges is equal to load instructions
          load_edge_sum = load_edges.map { |me| [me,1] }
          ipet_builder.mc_model.assert_equal(load_edge_sum, {li=>1}, "load_edges_#{li}",:cache)
        }
        # add variable for tag
        ipet_builder.ilp.add_variable(tag)

        # sum of all load instructions is equal to tag
        load_ins_sum = load_instructions.map { |li| [li,1] }
        ipet_builder.mc_model.assert_equal(load_ins_sum, {tag=>1}, "tag_#{tag}", :cache)

        # tag is less equal sum of all scopes
        scope_sum = @conflict_free_scopes[tag].map { |scope_node, f|
          [scope_node.scope_entry, f]
        }
        ipet_builder.mc_model.assert_less_equal({tag=>1}, scope_sum, "tagsum_#{tag}", :cache)
      }
    end
  end


  def analyze(scopegraph)
    @load_instructions = []
    @region_graphs = {}
    @conflict_free_scopes = {}
    @all_tags = {}

    # for each region node, we compute the cache access graph
    scopegraph.bottom_up.each { |n|
      if rg = n.region
        assert("Inconsistent region graph") {
          sub_scope_nodes = rg.nodes.select { |n| n.kind_of?(RegionGraph::SubScopeNode) }.map { |n| n.scope_node }
          Set[*sub_scope_nodes] == Set[*n.successors]
        }
        arg = rg.action_graph { |instruction|
          @cache.load_instructions(instruction).map { |li|
            @load_instructions.push(li)
            li
          }
        }
        @region_graphs[n] = arg
        #arg.dump (nice for debugging)
      end
    }

    # compute cache miss constraints
    0.upto(@cache.sets-1) do |set|
      debug(options, :cache) { "Starting cache region analysis for cache set #{set}" }
      compute_miss_constraints(scopegraph, set)
    end
  end

  #
  # This function collects all persistence scopes for one set,
  # or of the memory blocks of one set (LRU).
  # It traverse the scope graph bottom-up; conflict-free scopes are
  # just marked (dynamic programming), conflicting scopes are analyzed.
  #
  def compute_miss_constraints(scopegraph, set)
    @conflict_free = {}
    scopegraph.bottom_up.each { |node|
      if conflict_free?(node, set)
        # if the node is conflict free, just mark it (unless it is the entry)
        debug(options, :cache) { "Conflict-free Scope: #{node} #{set}" }
        if node == scopegraph.root
          get_all_tags(node, set).each { |tag, load_instructions|
            add_scope_for_tag(node, tag)
          }
        end
      else
        # analyze conflict scope
        compute_miss_constraints_in_conflict_scope(node, set)
      end
    }
  end

  #
  # Add miss constraints if there is a conflict
  #
  def compute_miss_constraints_in_conflict_scope(node, set)

    debug(options, :cache) { "Conflicts in scope #{node}: #{get_all_tags(node, set).keys.inspect}" }

    if rg = get_region_graph(node)
      if false && options.conflict_regions
        # not yet ported
      else
        # If this is a region node, add misses for all actions in the current set
        rg.action_nodes.each { |action_node|
          load_instruction = action_node.action
          tag = load_instruction.tag
          if @cache.set_of(tag) == set
            add_scope_for_tag(ScopeGraph::BlockNode.new(action_node.block, node.context), tag)
          end
        }
        add_conflict_free_subscope_constraints(node, set)
      end
    else
      add_conflict_free_subscope_constraints(node, set)
    end
  end

  def add_conflict_free_subscope_constraints(node, set)
    # add scope constraint for all conflict-free successors
    node.successors.each { |snode|
      debug(options, :cache) { "conflict_scope: subscope #{snode}, needs action: #{conflict_free?(snode,set)}" }
      if conflict_free?(snode, set)
        get_all_tags(snode,set).each { |tag, load_instructions|
          add_scope_for_tag(snode, tag)
        }
      end
    }
  end

  #
  # add conflict-free scope for tag
  #
  def add_scope_for_tag(node, tag)
    debug(options, :cache) { "add scope #{node} for tag #{tag}" }
    @conflict_free_scopes[tag] ||= Hash.new(0)
    @conflict_free_scopes[tag][node] += 1
  end

  #
  # decide whether a cache set is conflict free within one scope
  #
  def conflict_free?(node, set)
    return @conflict_free[[node,set]] if @conflict_free[[node,set]]
    @conflict_free[[node,set]] = @cache.conflict_free?(get_all_tags(node, set).keys)
  end

  #
  # get all tags accessed in one set
  #
  def get_all_tags(node, set)
    all_tags = @all_tags[[node,set]]
    return all_tags if all_tags
    all_tags = get_local_tags(node,set)
    node.successors.each { |succ|
      get_all_tags(succ, set).each { |t,is|
        all_tags[t] ||= Set.new
        all_tags[t] += is
      }
    }
    @all_tags[[node,set]] = all_tags
  end

  def get_local_tags(node,set)
    local_tags = {}
    region_graph = get_region_graph(node)
    return local_tags unless region_graph
    region_graph.action_nodes.each { |node|
      load_instruction = node.action
      tag = load_instruction.tag
      if @cache.set_of(tag) == set
        (local_tags[tag]||=Set.new).add(load_instruction)
      end
    }
    local_tags
  end

end


class MethodCacheAnalysis

  def initialize(cache, pml, options)
    @cache, @pml, @options = cache, pml, options
    @block_subfunction_map = {}
  end

  def block_capacity
    @cache.size / @cache.block_size
  end

  def sets
    1
  end

  def set_of(_)
    0
  end

  def subfunction_of_block(b)
    unless sf_of_block = @block_subfunction_map[b.function]
      sf_of_block = @block_subfunction_map[b.function] = {}
      b.function.subfunctions.each { |sf|
        sf.blocks.each { |b| sf_of_block[b] = sf }
      }
    end
    sf_of_block[b]
  end

  def load_instructions(i)
    function = i.function
    sf = subfunction_of_block(i.block)
    if i.index == 0 && sf.entry == i.block
       [LoadInstruction.new(i, sf)]
    elsif i.may_return_to?
       [LoadInstruction.new(i, sf)]
    else
       []
    end
  end

  # decision is correct for all variant of the method cache (fixed and variable block)
  def conflict_free?(subfunctions)

    # the number of blocks occupied by all subfunctions may not exceed the
    # number of blocks available in the cache
    c1 = subfunctions.map { |sf| blocks_for_tag(sf) }.inject(0,:+) <= block_capacity

    # the number of subfunctions in the cache may not exceed the
    # associativity of the cache
    c2 = subfunctions.length <= @cache.associativity

    c1 && c2
  end

  def load_cost(subfunction)
    @pml.arch.subfunction_miss_cost(subfunction)
  end

  def blocks_for_tag(subfunction)
    @cache.bytes_to_blocks(@pml.arch.subfunction_size(subfunction))
  end

end

class CacheLine
  include QNameObject
  attr_reader :address, :function
  def initialize(address, function)
    @address, @function = address, function
    @qname = "CacheLine: #{address}"
  end
  def to_s
    qname
  end
end

class InstructionCacheAnalysis

  attr_reader :cache

  def initialize(cache, pml, options)
    @cache, @pml, @options = cache, pml, options
    @offset_bits = Math.log2(cache.line_size).to_i
  end

  def sets
    @cache.size / (@cache.associativity * @cache.line_size)
  end

  def aligned_addr(addr)
    addr & ~(cache.line_size - 1)
  end

  def set_of(cache_line)
    (cache_line.address >> @offset_bits) & (sets - 1)
  end

  def get_aligned_addresses(first_address, last_address)
    addrs = []
    addr = aligned_addr(first_address)
    while addr <= last_address
      addrs.push(addr)
      addr += cache.line_size
    end
    addrs
  end

  # One tricky aspect here is that the fetch does not know the size
  # of the instruction; it is thus architecture dependent how many
  # words have to be fetched
  def load_instructions(i)
    prev = i.index == 0 ? nil : i.block.instructions[i.index-1]
    last_byte = i.address + @pml.arch.instruction_fetch_bytes - 1
    if prev.nil?
      same_cache_line_as_prev = false
    elsif aligned_addr(prev.address) != aligned_addr(i.address)
      same_cache_line_as_prev = false
    elsif aligned_addr(prev.address) != aligned_addr(last_byte)
      same_cache_line_as_prev = false
    else
      same_cache_line_as_prev = true
    end
    if ! same_cache_line_as_prev || i.may_return_to?
      get_aligned_addresses(i.address, last_byte).map { |addr|
        LoadInstruction.new(i, CacheLine.new(addr, i.function))
      }
    else
      []
    end
  end

  def conflict_free?(cache_lines)
    cache_lines.length <= cache.associativity
  end

  def load_cost(_cache_line)
    @pml.arch.config.main_memory.read_delay(cache.line_size)
  end

end

class StackCacheAnalysis
  def initialize(cache, pml, options)
    @cache, @pml, @options = cache, pml, options
  end
  def analyze(scope_graph)
    @fill_blocks, @spill_blocks = Hash.new(0), Hash.new(0)
    scope_graph.bottom_up.each { |n|
      next unless n.kind_of?(ScopeGraph::FunctionNode)
      f = n.function
      f.instructions.each { |i|
        if i.sc_fill.to_i > 0
          @fill_blocks[i] += i.sc_fill
        elsif i.sc_spill.to_i > 0
          @spill_blocks[i] += i.sc_spill
        end
      }
    }
  end
  def extend_ipet(ipet_builder)
    ilp = ipet_builder.ilp
    @fill_blocks.each { |instruction,fill_blocks|
      ipet_builder.mc_model.add_block_cost(instruction.block, @pml.arch.config.main_memory.read_delay(fill_blocks*@cache.block_size))
    }
    @spill_blocks.each { |instruction,spill_blocks|
      ilp_builder.mc_model.add_block_cost(instruction.block, @pml.arch.config.main_memory.write_delay(spill_blocks*@cache.block_size))
    }
  end
end


end # module PML
