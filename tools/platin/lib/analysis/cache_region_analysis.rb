#
# platin tool set
#
# == Cache Persistence Analysis via Conflict-Free Region Identification
#
# See Article [TODO]

require 'core/pml'
require 'analysis/scopegraph'
require 'analysis/cache_persistence_analysis'

module PML

class CacheAnalysis

  def initialize(refinement, pml, options)
    @pml, @options, @refinement = pml, options, refinement
  end

  def scope_graph(entry_function)
    @scope_graph = ScopeGraph.new(entry_function, @refinement, @pml, @options) unless @scope_graph
    @scope_graph
  end
  def analyze(entry_function, ipet_builder)
    @scope_graph = nil # reset, entry_function might have changed
    if mc = @pml.arch.method_cache and not @options.disable_ica
      @mca = CacheRegionAnalysis.new(MethodCacheAnalysis.new(mc, entry_function, @pml, @options), @pml, @options)
      @mca.extend_ipet(scope_graph(entry_function), ipet_builder)
    elsif ic = @pml.arch.instruction_cache and not @options.disable_ica
      @ica = CacheRegionAnalysis.new(InstructionCacheAnalysis.new(ic, @pml, @options), @pml, @options)
      @ica.extend_ipet(scope_graph(entry_function), ipet_builder)
    end
    if sc = @pml.arch.stack_cache and not @options.disable_sca
      if @options.use_sca_graph
        @sca = StackCacheAnalysisGraphBased.new(sc, @pml, @options)
      else
        @sca = StackCacheAnalysis.new(sc, @pml, @options)
      end
      @sca.analyze_nonscope()
      @sca.extend_ipet(ipet_builder)
    end
    if dm = @pml.arch.data_memory and not dm.ideal? and not @options.disable_dca
      # Note: We also run the data cache analysis if there is no data-cache configured, since we
      #       also add the costs for uncached loads and stores here!
      # Note: @options.disable_dca disables *all* data memory access costs and assumes *0* costs.
      #       To disable only the data-cache analysis itself, set the cache analysis type to always-miss.
      #       (for future reference)
      # TODO This is a bit misleading; find a way to add costs for uncached memory accesses outside of
      #      this analysis, only handle the case 'if @pml.arch.data_cache' here?! Maybe rename option to
      #      'disable_dma' and move this whole code into a separate DataMemoryAnalysis class.
      # TODO We should instantiate an additional cache analysis for local memory accesses (stack, d-spm, IO-devices).
      #      Instantiating the cache analyses should be driven by @pml.arch, so that it can create the proper
      #      set of analyses based on the architecture (M$, I$, D$, S$, SPM, ..) and matches the way @pml.arch
      #      calculates basic block timings (with or without latencies for bypass memory accesses,..).
      #      The DataCacheAnalysis classes should take a functor block that determines which instructions
      #      should be handled by that instance of the cache analysis (instead of checking @pml.arch.data_cache_access?)
      dc = @pml.arch.data_cache
      always_hit = false
      always_hit = :cached if @options.dca_analysis_type == 'always-hit'
      # An ideal D$ always hits on both loads *and* stores.
      always_hit = :all    if (dc && dc.ideal?)
      if not dc or always_hit or @options.dca_analysis_type == 'always-miss'
        @dca = AlwaysMissCacheAnalysis.new(NoDataCacheAnalysis.new(dm, always_hit, @pml, @options), @pml, @options)
      else
        @dca = CacheRegionAnalysis.new(DataCacheAnalysis.new(dm, dc, @pml, @options), @pml, @options)
      end
      @dca.extend_ipet(scope_graph(entry_function), ipet_builder)
    end
  end

  def summarize(options, freqs, cost, report)
    total_cycles = 0
    ica_results, sca_results = Hash.new(0), Hash.new(0)
    
    puts "Method cache contribution:" if @mca and options.verbose
    ica_results = @mca.summarize(options, freqs, cost) if @mca

    puts "Instruction cache contribution:" if @ica and options.verbose
    ica_results = @ica.summarize(options, freqs, cost) if @ica

    puts "Stack cache contribution:" if @sca and options.verbose
    sca_results = @sca.summarize(options, freqs, cost) if @sca

    puts "Data cache contribution:" if @dca and options.verbose
    dca_results = @dca.summarize(options, freqs, cost) if @dca

    { "instr" => ica_results, "stack" => sca_results, "data" => dca_results }.each { |type,r|
      r.each { |k,v|
        report.attributes[k + "-" + type] = v
	total_cycles += v if k == "cache-max-cycles"
      } if r
    }
    
    report.attributes['cache-max-cycles'] = total_cycles
  end
end

# +LoadInstruction+ objects represent the load of a certain memory block
# at a certain program point.
class LoadInstruction
  include QNameObject
  attr_reader :insref, :tag, :store, :bypass
  def initialize(insref, tag, store=false, bypass=false)
    @insref, @tag, @store, @bypass = insref, tag, store, bypass
    @qname = "#{@tag.qname}@#{insref.qname}"
  end
  def function
    @insref.function
  end
  def store?
    return @store
  end
  def bypass?
    return @bypass
  end
  # True if the accessed address is known precisely
  def known?
    return false unless tag and tag.respond_to?(:known?)
    tag.known?
  end
  # True if the address is not known at all (no range or symbol)
  def unknown?
    return false unless tag and tag.respond_to?(:unknown?)
    tag.unknown?
  end
  def to_s
    "#{tag}@#{insref}"
  end
  def inspect
    sprintf("#<LoadInstruction:0x%8x %s>",self.object_id, self.to_s)
  end
end

# +MemoryEdge+ is a variable to account for the load of a certain memory
# block at a certain program point, attributed to a certain edge of the
# incoming block the program point is contained in
class MemoryEdge
  include QNameObject
  attr_reader :edgeref,:load_instruction
  def initialize(edgeref, load_instruction)
    @edgeref, @load_instruction = edgeref, load_instruction
    @qname = "#{edgeref.qname}->#{load_instruction.qname}"
  end
  def function
    @load_instruction.insref.function
  end
  def to_s
    @qname = "#{@edgeref}->#{@load_instruction}"
  end
end

class CacheAnalysisBase
  def summarize(options, freqs, cost)
    cycles = 0
    misses = 0
    hits = 0
    stores = 0
    bypasses = 0
    known = 0
    unknown = 0
    @all_load_edges.each { |me|
      li = me.load_instruction
      puts "  cache load edge #{me}: #{freqs[me] || '??'} / #{freqs[me.edgeref] || '??'} (#{cost[me]} cyc)" if options.verbose
      cycles += cost[me] || 0
      # count store and bypass separately
      misses += freqs[me] || 0 unless li.bypass? or li.store?
      hits += (freqs[me.edgeref] || 0) - (freqs[me] || 0) unless li.bypass? or li.store?
      # TODO depending on the cache, we might count stores as hits+misses too..
      stores += freqs[me] || 0 if li.store?
      bypasses += freqs[me] || 0 if li.bypass?
      # count known and unknown accesses indepenently
      known += freqs[me] || 0 if li.known?
      unknown += freqs[me] || 0 if li.unknown?
    }
    { "cache-max-cycles" => cycles, "cache-min-hits" => hits, "cache-max-misses" => misses,
      "cache-max-stores" => stores, "cache-max-bypass" => bypasses,
      "cache-known-address" => known, "cache-unknown-address" => unknown 
    }.select { |k,v| v > 0 or %w[cache-max-cycles cache-max-misses cache-min-hits].include?(k) }.map { |k,v| [k,v.to_i] }
  end
end

#
# A simple cache analysis driver implementation that does not check
# for conflicts, it just adds all the costs to the IPET.
# 
class AlwaysMissCacheAnalysis < CacheAnalysisBase

  attr_reader :pml, :options, :cache_properties

  def initialize(cache_properties, pml, options)
    @cache_properties, @pml, @options = cache_properties, pml, options
  end

  # extend IPET with load costs
  def extend_ipet(scopegraph, ipet_builder)

    # all memory edges 
    @all_load_edges = []

    # iterate over all instructions in the call graph
    scopegraph.bottom_up.each { |n|
      # TODO Handle SCCNodes from recursive calls
      next unless n.kind_of?(ScopeGraph::FunctionNode)

      n.function.blocks.each { |block|
        block.instructions.each { |i|
	 
	  load_instructions = @cache_properties.load_instructions(i)
	  next unless load_instructions

          load_instructions = load_instructions.to_a
          next if load_instructions.empty?

          debug(options,:cache) { "Load instructions for instruction: #{i}: #{load_instructions.join(",")}" }

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
	      ipet_builder.ilp.add_cost(me, @cache_properties.load_cost(li.tag))
              debug(@options, :cache, :costs) { "Costs for load edge #{me}: #{@cache_properties.load_cost(li.tag)}" }
	      # memory edge frequency is less equal to edge frequency
	      ipet_builder.mc_model.assert_less_equal({me=>1},{me.edgeref=>1},"load_edge_#{me}",:cache)
	      load_edges.push(me)
	    }
	    # sum of load edges is equal to load instructions
	    load_edge_sum = load_edges.map { |me| [me,1] }
	    ipet_builder.mc_model.assert_equal(load_edge_sum, {li=>1}, "load_edges_#{li}",:cache)
	    # collect all load edges
	    @all_load_edges.concat(load_edges)
	  }
	}
      }
    }
  end

end

#
# Cache Region Analyses proceed in two steps,
# identification of conflict-free scopes (or persistence scopes for
# LRU analysis) and modification of the ipet to account for memory
# transfer costs.
#
class CacheRegionAnalysis < CacheAnalysisBase

  # cache tags
  class Tag
    include QNameObject
    def initialize(qname, set)
      @qname = "Tag:#{qname}"
      @set = set
    end
  end

  attr_reader :pml, :options, :cache_properties

  def initialize(cache_properties, pml, options)
    @cache_properties, @pml, @options = cache_properties, pml, options
  end

  # extend IPET, using conflict free scopes computed by +analyze+
  def extend_ipet(scopegraph, ipet_builder)

    # cache tags per scope
    @all_tags = {}

    # all memory edges 
    @all_load_edges = []

    # run scope-based analysis
    conflict_free_scopes = analyze(scopegraph)

    # modify IPET
    0.upto(@cache_properties.sets) do |set|

      get_all_tags(scopegraph.root, set).each { |tag,load_instructions|

        load_instructions = load_instructions.to_a
        debug(options,:cache) { "Load instructions for tag #{tag}: #{load_instructions.join(",")}" }
        debug(options,:cache) {
          "Scopes for tag #{tag}: #{conflict_free_scopes[tag]}"
        }
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
            ipet_builder.ilp.add_cost(me, @cache_properties.load_cost(tag))
            debug(@options, :cache, :costs) { "Costs for load edge #{me}: #{@cache_properties.load_cost(li.tag)}" }
            # memory edge frequency is less equal to edge frequency
            ipet_builder.mc_model.assert_less_equal({me=>1},{me.edgeref=>1},"load_edge_#{me}",:cache)
            load_edges.push(me)
          }
          # sum of load edges is equal to load instructions
          load_edge_sum = load_edges.map { |me| [me,1] }
          ipet_builder.mc_model.assert_equal(load_edge_sum, {li=>1}, "load_edges_#{li}",:cache)
	  # collect all load edges
	  @all_load_edges.concat(load_edges)
        }

        # add variable for tag
        ipet_builder.ilp.add_variable(tag)

        # sum of all load instructions is equal to tag
        load_ins_sum = load_instructions.map { |li| [li,1] }
        ipet_builder.mc_model.assert_equal(load_ins_sum, {tag=>1}, "tag_#{tag}", :cache)

        # tag is less equal sum of all scopes
        scope_sum = conflict_free_scopes[tag].map { |scope_node, f|
          [scope_node.scope_entry, f]
        }
        ipet_builder.mc_model.assert_less_equal({tag=>1}, scope_sum, "tagsum_#{tag}", :cache)
      }
    end
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

  #
  # get all tags accessed locally (in this scope excluding subscopes),
  # restricted to the specified set
  #
  def get_local_tags(node,set)
    local_tags = {}
    region_graph = get_region_graph(node)
    return local_tags unless region_graph
    region_graph.action_nodes.each { |node|
      load_instruction = node.action
      tag = load_instruction.tag
      if @cache_properties.set_of(tag) == set
        (local_tags[tag]||=Set.new).add(load_instruction)
      end
    }
    local_tags
  end


  #
  # scope-based cache analysis
  # first builds region graphs, then computes conflict-free scopes
  # for each memory block (tag)
  #
  def analyze(scopegraph)

    @region_graphs = {}
    @conflict_free_scopes = {}

    # for each region node, we compute the cache access graph
    scopegraph.bottom_up.each { |n|
      if rg = n.region
        assert("Inconsistent region graph") {
          sub_scope_nodes = rg.nodes.select { |n| n.kind_of?(RegionGraph::SubScopeNode) }.map { |n| n.scope_node }
          Set[*sub_scope_nodes] == Set[*n.successors]
        }
        arg = rg.action_graph { |instruction|
          @cache_properties.load_instructions(instruction)
        }
        @region_graphs[n] = arg
        # arg.dump #(nice for debugging)
      end
    }

    # compute cache miss constraints
    all_tags = []
    0.upto(@cache_properties.sets-1) do |set|
      debug(options, :cache) { "Starting cache region analysis for cache set #{set}" }
      compute_miss_constraints(scopegraph, set)
      all_tags.concat(get_all_tags(scopegraph.root, set).keys)
    end

    # hack for evaluation purposes only
    $imem_bytes = @cache_properties.size_in_bytes(all_tags) if @cache_properties.name == "I$" || @cache_properties.name == "M$"
    statistics("CACHE", "size of all reachable memory blocks for #{@cache_properties.name} (bytes)" =>
               @cache_properties.size_in_bytes(all_tags)) if options.stats
    # cache = @cache_properties.cache
    # all_tags.each { |tag|
    #   info "#{tag.inspect} -> #{tag.size} -> #{cache.bytes_to_blocks(tag.size) * cache.block_size}"
    # }
    # info $imem_bytes

    @conflict_free_scopes
  end

  #
  # get region graph for scope graph node
  #
  def get_region_graph(node)
    @region_graphs[node]
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
  # This function collects all conflict-free scopes for one set,
  # or of the memory blocks of one set (LRU).
  # It traverse the scope graph bottom-up; conflict-free scopes are
  # just marked (dynamic programming), conflicting scopes are analyzed.
  #
  def compute_miss_constraints(scopegraph, set)
    if persistence_analysis?
      PersistenceAnalysis.new(self, options).compute_conflict_free_scopes(scopegraph, set)
    else
      ConflictAnalysis.new(self, options).compute_conflict_free_scopes(scopegraph, set)
    end
  end

  def persistence_analysis?
    @cache_properties.cache.policy == "lru" && options.wca_persistence_analysis
  end

end

#
# class to compute conflict-free regions
#
class ConflictFreeRegionFormation
  attr_reader :analysis, :cache_set

  def initialize(region_graph, set, analysis)
    @rg, @analysis = region_graph, analysis
    @cache_set = set
    @only_simple_regions = false
    @regions, @region_header, @region_nodes = [], {}, {}, {}
  end

  def build_regions
    # NOTE: ruby tsort is recursive, and does not work for large graphs :(
    # therefore, we use our own, efficient implementation
    topological_sort(@rg.entry_node).each { |node|
      if(pred_region = expandable?(node))
        join_regions(pred_region, node)
      else
        new_region(node)
      end
    }
    self
  end

  def new_region(node)
    @region_header[node] = node
    @region_nodes[node] = [node]
    @regions.push(node)
  end

  def join_regions(pred_region, node)
    @region_header[node] = pred_region
    @region_nodes[pred_region].push(node)
  end

  # yields region headers
  def each
    @regions.each { |region_header|
      yield region_header
    }
  end

  def nodes_of_region(header)
    @region_nodes[header]
  end

  def scope_of_region(region_header, scope_node)
    case region_header
    when RegionGraph::BlockEntryNode
      ScopeGraph::BlockNode.new(region_header.block, scope_node.context)
    when RegionGraph::BlockSliceNode
      ScopeGraph::BlockNode.new(region_header.block, scope_node.context)
    when RegionGraph::ActionNode
      ScopeGraph::BlockNode.new(region_header.block, scope_node.context)
    when RegionGraph::SubScopeNode
      region_header.scope_node
    else
      # should never be reached, as entry, exit and recursion nodes are isolated without accesses
      raise Exception.new("#{region_header} of type #{region_node.class} does not correspond to a scope")
    end
  end

  private

  def expandable?(node)
    return nil if node.kind_of?(RegionGraph::RecNode)
    return nil if node.kind_of?(RegionGraph::ExitNode)
    if node.kind_of?(RegionGraph::SubScopeNode)
      return nil unless analysis.conflict_free?(node.scope_node, cache_set)
    end
    some_pred = node.predecessors.first
    return nil if some_pred.nil?
    return nil if node.predecessors.length > 1 && @only_simple_regions
    pred_region = @region_header[some_pred]

    return nil if node.predecessors.any? { |pred| pred.kind_of?(RegionGraph::EntryNode) }
    return nil if node.predecessors.any? { |pred| @region_header[pred] != pred_region }

    return nil unless expanded_conflict_free?(pred_region, node)
    return pred_region
  end
end

class ConflictAnalysis

  class RegionFormationRA < ConflictFreeRegionFormation
    def initialize(region_graph, cache_set, analysis)
      super(region_graph, cache_set, analysis)
      @region_tags = {}
    end
    def new_region(node)
      super(node)
      @region_tags[node] = Set[*get_node_tags(node)]
    end
    def join_regions(pred_region, new_node)
      super(pred_region, new_node)
      @region_tags[pred_region] += get_node_tags(new_node)
    end
    def tags_of_region(region)
      @region_tags[region] || Set.new
    end
    def expanded_conflict_free?(pred_region, node)
      pred_tags = @region_tags[pred_region]
      node_tags = get_node_tags(node)
      return analysis.cache_properties.conflict_free?(pred_tags + node_tags)
    end
    def get_node_tags(node)
      if node.kind_of?(RegionGraph::ActionNode)
        tag = node.action.tag
        return [] unless analysis.cache_properties.set_of(tag) == cache_set
        Set[tag]
      elsif node.kind_of?(RegionGraph::SubScopeNode)
        return [] unless analysis.conflict_free?(node.scope_node, cache_set)
        Set[*analysis.get_all_tags(node.scope_node, cache_set).keys]
      else
        Set.new
      end
    end
  end
  attr_reader :options

  def initialize(cache_analysis, options)
    @analysis, @options = cache_analysis, options
  end

  # delegator to CacheRegionAnalysis#get_all_tags
  def get_all_tags(node, set)
    @analysis.get_all_tags(node, set)
  end

  # delegator to CacheRegionAnalysis#cache_properties
  def cache_properties
    @analysis.cache_properties
  end

  def compute_conflict_free_scopes(scopegraph, set)
    @conflict_free = {}
    scopegraph.bottom_up.each { |node|
      if conflict_free?(node, set)
        # if the node is conflict free, just mark it (unless it is the entry)
        debug(options, :cache) { "Conflict-free Scope: #{node} #{set}" }
        if node == scopegraph.root
          get_all_tags(node, set).each { |tag, load_instructions|
            @analysis.add_scope_for_tag(node, tag)
          }
        end
      else
        # analyze conflict scope
        self.analyze_conflict_scope(node, set)
      end
    }
  end

  #
  # decide whether a cache set is conflict free within one scope
  #
  def conflict_free?(node, set)
    return @conflict_free[[node,set]] if @conflict_free[[node,set]]
    @conflict_free[[node,set]] = cache_properties.conflict_free?(get_all_tags(node, set).keys)
  end

  #
  # Find conflict-free sub scopes in a scope with conflicts
  #
  def analyze_conflict_scope(node, set)

    debug(options, :cache) { "Conflicts in scope #{node}: #{get_all_tags(node, set).keys.inspect}" }

    if rg = @analysis.get_region_graph(node)
      if options.wca_cache_regions
        # process region graph in topological order
        rf = RegionFormationRA.new(rg, set, self)
        rf.build_regions.each { |header|
          tags = rf.tags_of_region(header)
          next if tags.empty?
          scope_node = rf.scope_of_region(header, node)
          tags.each { |tag|
            @analysis.add_scope_for_tag(scope_node, tag)
          }
        }
      else
        # If this is a region node, add misses for all actions in the current set
        rg.action_nodes.each { |action_node|
          load_instruction = action_node.action
          tag = load_instruction.tag
          if cache_properties.set_of(tag) == set
            @analysis.add_scope_for_tag(ScopeGraph::BlockNode.new(action_node.block, node.context), tag)
          end
        }
        add_conflict_free_subscopes(node, set)
      end
    else
      add_conflict_free_subscopes(node, set)
    end
  end


  def add_conflict_free_subscopes(node, set)
    # add scope constraint for all conflict-free successors
    node.successors.each { |snode|
      debug(options, :cache) { "conflict_scope: subscope #{snode}, needs action: #{conflict_free?(snode,set)}" }
      if conflict_free?(snode, set)
        get_all_tags(snode,set).each { |tag, load_instructions|
          @analysis.add_scope_for_tag(snode, tag)
        }
      end
    }
  end

end

class MethodCacheAnalysis

  def name
    "M$"
  end

  attr_reader :cache
  def initialize(cache, entry_function, pml, options)
    @cache, @pml, @options = cache, pml, options
    @block_subfunction_map = {}
    @entry_function = entry_function
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
    # Load subfunction on entry (but skip the initial load when entering the target function)
    # TODO Do not skip the load instruction for recursive target function calls.
    #      This requires us to detect if we are in a region node for the root function of the scope graph.
    #      At the moment, recursive functions are not supported anyway though.
    if i.index == 0 && sf.entry == i.block && 
       (i.block != @entry_function.entry_block || @options.target_callret_costs)
       [LoadInstruction.new(i, sf)]
    # Load subfunction when returning into the subfunction after a call site.
    elsif i.may_return_to?
       [LoadInstruction.new(i, sf)]
    else
       []
    end
  end

  # decision is correct for all variant of the method cache (fixed and variable block)
  def conflict_free?(subfunctions)
    return false if @options.wca_minimal_cache
    return true  if @options.wca_ideal_cache

    # the number of blocks occupied by all subfunctions may not exceed the
    # number of blocks available in the cache
    c1 = subfunctions.map { |sf| blocks_for_tag(sf) }.inject(0,:+) <= block_capacity

    # the number of subfunctions in the cache may not exceed the
    # associativity of the cache
    c2 = subfunctions.length <= @cache.associativity

    c1 && c2
  end

  def size_in_bytes(subfunctions)
    subfunctions.map { |sf| @cache.bytes_to_blocks(sf.size) * @cache.block_size }.inject(0,:+)
  end

  def load_cost(subfunction)
    @pml.arch.subfunction_miss_cost(subfunction)
  end

  def blocks_for_tag(subfunction)
    @cache.bytes_to_blocks(subfunction.size)
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

  def name
    "I$"
  end

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

  def size_in_bytes(cache_lines)
    @cache.line_size * cache_lines.length
  end

  def conflict_free?(cache_lines)
    return false if @options.wca_minimal_cache
    return true  if @options.wca_ideal_cache

    cache_lines.length <= cache.associativity
  end

  def load_cost(cache_line)
    d = @pml.arch.config.main_memory.read_delay(cache_line.address, cache.line_size)
    # info("read_delay for cache_line at #{cache_line.address} (#{cache_line.address & (@cache.line_size-1)}): #{d}")
    d
  end
end



class DataCacheLine
  include QNameObject
  
  attr_reader :address, :function, :memmode, :memtype
  def initialize(address, function, memmode, memtype)
    @address, @function, @memmode, @memtype = address, function, memmode, memtype
    # TODO For now the cache analysis does not support that the same cache line (equal qname) appears in
    #      multiple sets, since it tries to add a new variable to the ILP per set. 
    #      As a workaround we need to ensure that an (unknown) cache line in different sets has different names.
    if not address
      @qname = "CacheLine: unknown"
      if uncached?
        @qname = "#{qname} (uncached)"
      end
    else
      @qname = "CacheLine: #{address}"
    end
  end
  def bypass?
    memtype == 'memory'
  end
  def store?
    memmode == 'store'
  end
  # True if the accessed address is known precisely
  def known?
    address
  end
  # True if the address is not known at all (no range or symbol)
  def unknown?
    address.nil?
  end
  def uncached?
    bypass? or store?
  end
  def to_s
    qname
  end
end

#
# Base class for data cache analysis implementations
#
class DataCacheAnalysisBase
  attr_reader :memory, :line_size

  def initialize(memory, pml, options)
    @memory, @pml, @options = memory, pml, options
  end

  def name
    "D$"
  end

  def aligned_addr(addr)
    addr & ~(@line_size - 1)
  end

  def get_aligned_addresses(first_address, last_address)
    addrs = []
    addr = aligned_addr(first_address)
    while addr <= last_address
      addrs.push(addr)
      addr += @line_size
    end
    addrs
  end

  def load_cost(cache_line)
    if cache_line.store?
      if cache_line.known?
        d = @memory.write_delay(cache_line.address, @line_size)
      else
        # Note: We assume here that we do not have unaligned stores.
        d = @memory.write_delay_aligned(@line_size)
      end
    else
      if cache_line.known?
        d = @memory.read_delay(cache_line.address, @line_size)
      else
        # Note: We assume here that we do not have
        # unaligned accesses across multiple cache lines.
        d = @memory.read_delay_aligned(@line_size)
      end
    end
    # info("read_delay for cache_line at #{cache_line.address} (#{cache_line.address & (@cache.line_size-1)}): #{d}")
    d
  end
end

#
# Data access analysis for setups without a data-cache
# 
class NoDataCacheAnalysis < DataCacheAnalysisBase

  attr_reader :always_hit

  def initialize(memory, always_hit, pml, options)
    super(memory, pml, options)
    @always_hit = always_hit
    @line_size = @pml.arch.max_data_transfer_bytes
  end

  def sets
    1
  end

  def set_of(cache_line)
    0
  end

  def always_hit(line)
    # Bypass always misses
    return false if line.bypass?
    # All other (including stores) hit in an ideal cache
    return true  if @always_hit == :all 
    # Otherwise all loads hit if we use an always-hit analysis
    @always_hit == :cached and not line.uncached?
  end

  def load_instructions(i)
    if i.memmode == 'load' or i.memmode == 'store'
      return [] if not @pml.arch.data_cache_access?(i)
      line = DataCacheLine.new(nil, i.function, i.memmode, i.memtype)
      # Skip data-cache loads in case we are in always-hit mode..
      return [] if always_hit(line)
      # .. but handle stores and bypass loads nevertheless.
      [LoadInstruction.new(i, line, line.store?, line.bypass?)]
    else
      []
    end
  end

  def conflict_free?(cache_lines)
    false
  end
end

# 
# Data-cache analysis plugin for scope based cache analysis
#
class DataCacheAnalysis < DataCacheAnalysisBase

  attr_reader :cache

  def initialize(memory, cache, pml, options)
    super(memory, pml, options)
    @cache = cache
    @line_size = cache.line_size
    @offset_bits = Math.log2(@line_size).to_i 
  end

  def sets
    # TODO for now, we only have two sets: (unknown) accesses to data-cache, and
    #      all bypasses and stores
    2

    # We put bypass loads and stores to a separate set
    # TODO we could also run two separate analyses for cached and uncached accesses..
    #@cache.size / (@cache.associativity * @cache.line_size) + 1
  end

  def set_of(cache_line)
    return sets - 1 if cache_line.uncached?
    # TODO all other accesses go to set 0 for now.
    #      For unknown accesses we should instead return a set of
    #      all set-ids, and the cache analysis must put the cache line
    #      into all those sets (same for ranges).
    0
    #(cache_line.address >> @offset_bits) & (sets - 1)
  end

  def size_in_bytes(cache_lines)
    @line_size * cache_lines.length
  end

  def load_instructions(i)
    if i.memmode == 'load' or i.memmode == 'store'
      # TODO try to determine address (range) of access
      return [] if not @pml.arch.data_cache_access?(i)
      line = DataCacheLine.new(nil, i.function, i.memmode, i.memtype)
      [LoadInstruction.new(i, line, line.store?, line.bypass?)]
    else
      []
    end
  end

  def conflict_free?(cache_lines)
    return false if @options.wca_minimal_cache
    return true  if @options.wca_ideal_cache
    
    # Check if any set entry is an uncached acccess
    # We could also check for set-id, if we would get it here
    cache_lines.each { |cache_line|
      return false if cache_line.uncached?

      # We need to be conservative here if we do not know the address exactly,
      # because we do not know *how many* times this access is performed in this scope, and
      # each access might be a *different* unknown access.
      # TODO we could be less conservative here if we do not know the address but know that it will 
      #      always be the same value (eg a symbol), or if this access is (only) outside any loop within its
      #      conflict scope. We need to distinguish different unknowm accesses for that, i.e. the cacheline 
      #      tag (qname) must be different for constant and truly unknown accesses.
      return false unless cache_line.known?
    }

    cache_lines.length <= @cache.associativity
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
          @spills[i] += i.sc_spill
        end
      }
    }
  end
  def analyze_nonscope()
    @fill_blocks, @spill_blocks = Hash.new(0), Hash.new(0)
    @pml.machine_functions.each { |f|
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
    extend_ipet_fills(ipet_builder)
    extend_ipet_spills(ipet_builder)
  end
  def extend_ipet_fills(ipet_builder)
    @fill_blocks.each { |instruction,fill_blocks|
      begin
        delay = @pml.arch.config.main_memory.read_delay(0, fill_blocks*@cache.block_size)
        debug(@options, :cache, :costs) { "Cost for stack cache fill: #{fill_blocks}*#{@cache.block_size}=#{delay}" }
        #ipet_builder.mc_model.add_block_cost(instruction.block, delay)
        ipet_builder.mc_model.add_instruction(instruction)
        ipet_builder.ilp.add_cost(instruction, delay)
      rescue PML::UnknownVariableException
        warn("cannot annotate inst #{instruction}: #{$!}")
      end
    }
  end
  def extend_ipet_spills(ipet_builder)
    @spill_blocks.each { |instruction,spill_blocks|
      begin
        delay = @pml.arch.config.main_memory.write_delay(0, spill_blocks*@cache.block_size)
        debug(@options, :cache, :costs) { "Cost for stack cache spill: #{spill_blocks}*#{@cache.block_size}=#{delay}" }
        #ipet_builder.mc_model.add_block_cost(instruction.block, delay)
        ipet_builder.mc_model.add_instruction(instruction)
        ipet_builder.ilp.add_cost(instruction, delay)
      rescue PML::UnknownVariableException
        warn("cannot annotate inst #{instruction}: #{$!}")
      end
    }
  end
  def summarize(options, freqs, cost)
    fills = summarize_fills(options, freqs, cost)
    spills = summarize_spills(options, freqs, cost)
    fills.merge!(spills) { |k, v1, v2| v1 + v2 }
  end
  def summarize_fills(options, freqs, cost)
    cycles = 0
    misses = 0
    @fill_blocks.each { |v,_|
      puts "  sc fill #{v}: #{freqs[v] || '??'} (#{cost[v]} cyc)" if options.verbose
      cycles += cost[v] || 0
      misses += freqs[v] || 0
    }
    { "cache-max-cycles" => cycles, "cache-max-misses" => misses }
  end
  def summarize_spills(options, freqs, cost)
    cycles = 0
    misses = 0
    @spill_blocks.each { |v,_|
      puts "  sc spill #{v}: #{freqs[v] || '??'} (#{cost[v]} cyc)" if options.verbose
      cycles += cost[v] || 0
      misses += freqs[v] || 0
    }
    { "cache-max-cycles" => cycles, "cache-max-misses" => misses }
  end
end

class IPETEdgeSCA < IPETEdge
  attr_reader :source, :target
  def initialize(edge_source, edge_target, cs)
    @source,@target,@cs = edge_source, edge_target, cs
    @level = "machinecode"
    arrow  = "=>"
    @qname = "#{@source.qname}#{arrow}#{@target.qname}::#{cs}"
  end
  def to_s
    "#{@qname}->#{@target.function}"
  end
  def cfg_edge?
    return false
  end
end

class StackCacheAnalysisGraphBased < StackCacheAnalysis
  def initialize(cache, pml, options)
    @cache, @pml, @options = cache, pml, options
  end
  def extend_ipet_spills(ipet_builder)
    ilp = ipet_builder.ilp
    @nodes = {}
    @spills = {}
    preds, succs = {}, {}
    assert("missing sca graph data") { @pml.sca_graph }
    @pml.sca_graph.nodes.each { |node|
      @nodes[node.id] = node
      #@mc_model.ipet.add_variable(node.id, :sca)
    }
    @pml.sca_graph.edges.each { |edge|
      srcn = @nodes[edge.src]
      dstn = @nodes[edge.dst]
      f = srcn.function
      bb = f.blocks.find { |bb| bb.data['mapsto'] == edge.block }
      cs = bb.instructions[edge.inst]
      assert("not a call: #{cs}") { cs.calls? }
      next unless ilp.has_variable?(IPETEdge.new(cs, dstn.function, :machinecode))
      debug(@options, :cache) { "sca edge for cs #{cs} and callees #{cs.callees}" }
      (succs[srcn] ||= []) << edge
      (preds[dstn] ||= []) << edge
      (@spills[[cs,dstn.function]] ||= []) << IPETEdgeSCA.new(srcn, dstn, cs)
    }
    @spills.values.flat_map { |i| i }.each { |e|
      ilp.add_variable(e)
      next if e.target.size == 0
      # TODO we should get the memory for the 'data' area
      delay = @pml.arch.config.main_memory.write_delay(0, e.target.size*@cache.block_size)
      debug(@options, :cache, :costs) { "Cost for stack cache spill: #{e.target.size}*#{@cache.block_size}=#{delay}" }
      ilp.add_cost(e, delay)
    }
    #puts ipet_builder.ilp.variables.select{ |v| v.kind_of? IPETEdge and v.call_edge? }
    ipet_builder.call_edges.each { |ce|
      k = [ce.source,ce.target]
      puts "k: #{k}"
      puts "ce: #{ce}"
      puts "sca: #{@spills[k]}"
      lhs = [[ce,1]] + @spills[k].map { |e| [e,-1] }
      ilp.add_constraint(lhs, "equal", 0, "sca_link", :sca)
    }
  end
  def summarize_spills(options, freqs, cost)
    cycles = 0
    misses = 0
    @spills.values.flat_map { |i| i }.each { |v|
      puts "  sc spill #{v}: #{freqs[v]} (#{cost[v]} cyc)" if freqs[v] and freqs[v] > 0 if options.verbose
      cycles += cost[v] || 0
      misses += freqs[v] || 0
    }
    { "cache-max-cycles" => cycles, "cache-max-misses" => misses }
  end
end

end # module PML
