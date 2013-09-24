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
      mca = MethodCacheAnalysis.new(mc, @pml, @options)
      mca.analyze(scope_graph)
      mca.extend_ipet(ipet_builder)
    elsif ic = @pml.arch.instruction_cache
      ica = InstructionCacheAnalysis.new(ic, @pml, @options)
      ica.analyze(scope_graph)
      ica.extend_ipet(ipet_builder)
    end
    if sc = @pml.arch.stack_cache
      sca = StackCacheAnalysis.new(sc, @pml, @options)
      sca.analyze(scope_graph)
      sca.extend_ipet(ipet_builder)
    end
    if dc = @pml.arch.data_cache
      dca = DataCacheAnalysis.new(dc, @pml, @options)
      dca.analyze(scope_graph)
      dca.extend_ipet(ipet_builder)
    end
  end
end

#
# Cache Region Analyses proceed in two steps,
# namely +analyze+ and +extend_ipet+.
#
# The purpose of +analyze+ is to analyze the cache
# behavior of the the given scope graph.
#
# A subsequent call to +extend_ipet+ modifies a given
# IPET problem such that it accounts for memory transfer
# costs.
#
class CacheRegionAnalysis
  attr_reader :pml, :options

  # access edges are IPET variables modelling memory accesses
  attr_reader :access_edges, :access_edges_of_tag

  def initialize(pml, options)
    @pml, @options = pml, options
  end

  # The purpose of +analyze+ is to analyze the cache
  # behavior of the the given scope graph. It usually
  # proceeds by (1) collecting all cache tags and accesses
  # to cache tags (2) collecting constraints that restrict
  # the frequency of cache misses.
  #
  # Constraint on the frequency of cache misses are represented
  # by assigning every tag a linear expression over +ScopeGraph+
  # nodes.
  #
  # For example,
  #  <tt>load_tag_constraints[X] = { F:compute => 2, L:main/3 => 1 }</tt>
  # means that the number of times +X+ is loaded into the cache is
  # bounded by the frequency of function +compute+ times 2 plus the
  # frequency of the loop entry edges of the loop <tt>main/3</tt>.
  #
  def analyze(scopegraph)

    # Unless overwritten, +analyze+ simply calls +record_tags+ and
    # +collect_tag_constraints+
    reset_analysis_results
    record_tags(scopegraph)
    collect_tag_constraints(scopegraph)
  end

  #
  # +extend_ipet modifies the IPET problem to account for memory transfer
  # costs (e.g., cache misses). It proceeds as follows:
  #
  #   (1) create variables that model the action of transferring a
  #       certain cache block at a certain program point
  #       (access variables)
  #
  #   (2) for every tag accessed in the scope graph
  #       - create variable that represent a cache miss
  #         when accessing the tag (tag variable)
  #       - add constraints that relate the frequency
  #         of the tag variable and the frequency of
  #         access variables, and add cost to access
  #         variables
  #       - add constraints that limit the frequency
  #         of tag variables according to the computed
  #         persistence information
  #
  #   (3) add additional cost (e.g., constant cost for cache bypass)
  #
  def extend_ipet(ipet_builder)
    add_access_variables(ipet_builder)
    @load_tag_constraints.each { |tag, bound|
      tag_var = add_variable_for_tag(tag, ipet_builder)
      miss_cost = miss_cost_for_tag(tag)
      debug(options, :cache) { "Cost for loading #{tag}: #{miss_cost}" }

      add_access_constraint(tag_var,  miss_cost, ipet_builder)
      add_region_constraint(tag_var, bound, ipet_builder)
    }
    add_additional_cost(ipet_builder)
  end

protected

  # This functions reset all analysis result variables
  def reset_analysis_results
    @load_tag_constraints = {}
    @access_edges, @access_edges_of_tag = Set.new, {}
  end

  # This function collects all tags and access variables, traversing
  # the scopegraph bottom-up
  def record_tags(scopegraph)
    scopegraph.bottom_up.each { |n|
      record_local_tags(n)
    }
  end

  # This function collects all tag constraints (for one cache set).
  # It traverse the scope graph bottom-up; conflict-free scopes are
  # just marked (dynamic programming), conflicting scopes are analyzed
  # using +analyze_conflic_scope+
  #
  def collect_tag_constraints(scopegraph)
    @all_tags = {}
    @conflict_free = {}
    scopegraph.bottom_up.each { |node|
      if conflict_free?(node)
        # if the node is conflict free, just mark it (unless it is the entry)
        debug(options, :cache) { "Conflict-free Scope: #{node} #{@set}" }
        if node == scopegraph.root
          get_all_tags(node).each { |t|
            increment_load_tag_bound(t,node)
          }
        end
      else
        # analyze conflict scope
        analyze_conflict_scope(node)
      end
    }
  end

  # This function analyzes a conflict scope, modifying +@load_tag_bounds+.
  #
  def analyze_conflict_scope(node)

    debug(options, :cache) { "Conflicts in scope #{node}: #{get_all_tags(node).inspect}" }

    # Currently, local tags in a conflict scope are assumed to be always miss.
    # Furthermore, locally each tag must be accessed at most once per scope.
    #
    # Consequently, it is necessary to use fine-grained scopes for instruction caches
    # (blocks and callsites) and data caches (memory access instructions).
    get_local_tags(node).each { |t|
      debug(options, :cache) { "Increment load tag bound (local miss): #{t} by #{node}" }
      increment_load_tag_bound(t, node)
    }

    # If there are no subscopes, we are done
    return if node.successors.empty?

    # We want to form "large" conflict-free single-entry subregions within the analyzed
    # scope. Here we use a simple greedy heuristic: traverse in topological order, and
    # expand the region formed by all predecessors, iff the predecessors are assigned to
    # the same region, and the expanded region is conflict free.
    #
    # For loops, note that the header is always the start of a region, and therefore
    # it is ok to ignore back-edges during region formation. Nevertheless, it might
    # be preferable to use regions for one loop iterations.
    #
    # For the region formation, we use the "count total distinct tags" heuristic to
    # decide whether the region is conflict free (which is also valid for loop scopes);
    # using a more precise decision procedure for acyclic regions
    # (e.g., count tag abstract domain) might further improve results.
    #
    if node.kind_of?(ScopeGraph::BlockNode) || node.kind_of?(ScopeGraph::FunctionNode) || node.kind_of?(ScopeGraph::LoopNode)
      subgraph = node.subgraph_adapter
      region, cf_regions = {}, {}

      topological_sort(subgraph.entry, subgraph).each { |subscope|

        # if the node is not conflict free, put it in a region by itself
        if ! conflict_free?(subscope)
          region[subscope] = subscope
        else
          subscope_tags = get_all_tags(subscope)
          pred_regions = subgraph.predecessors(subscope).map { |pred|
            Set[region[pred]]
          }.inject(Set.new) { |a,b| a+b }
          if pred_regions.size == 1
            # possibly expand predecessor region
            pred_region = pred_regions.to_a.first
            if pred_tags = cf_regions[pred_region]
              # predecessor region is conflict-free
              combined_tags = pred_tags + subscope_tags
              if tags_fit_in_cache?(combined_tags)
                # expand!
                region[subscope] = pred_region
                cf_regions[pred_region] = combined_tags
                next
              end # do not fit
            end # predecessor region is not conflict free
          end # different predecessor regions
          region[subscope] = subscope
          cf_regions[subscope] = subscope_tags
        end
      }
      cf_regions.each { |subscope, tags|
        debug(options, :cache) { "Conflict free sub-region in #{subscope} accessing #{tags.to_a}" }
        tags.each { |t|
          increment_load_tag_bound(t, subscope)
        }
      }
    else
      node.successors.each { |subscope|
        if conflict_free?(subscope)
          get_all_tags(subscope).each { |t|
            increment_load_tag_bound(t,node)
          }
        end
      }
    end
  end

  # compute whether a scope is conflict free
  def conflict_free?(scope_node)
    return @conflict_free[scope_node] unless @conflict_free[scope_node].nil?
    cf = if scope_node.successors.any? { |s| ! conflict_free?(s) }
           false
         else
           tags = get_all_tags(scope_node)
           if tags_fit_in_cache?(tags)
             true
           elsif conflict_free_scope?(scope_node)
             true
           else
             false
           end
         end
    @conflict_free[scope_node] = cf
  end

  def get_all_tags(scope_node)
    return @all_tags[scope_node] unless @all_tags[scope_node].nil?
    successor_tags = scope_node.successors.inject(Set.new) { |s,subscope| s + get_all_tags(subscope) }
    @all_tags[scope_node] = get_local_tags(scope_node) + successor_tags
  end

  def increment_load_tag_bound(tag, node)
    (@load_tag_constraints[tag]||=Hash.new(0))[node] += 1
  end

  def count_tags_dfa(scope_node, initial)
    # FunctionNode: transfer through collapsed CFG subgraph (topo order)
    # LoopNode: all accessed tags
    # BlockNode: access local tag (if any), transfer through all callsites
    r = case scope_node
    when ScopeGraph::FunctionNode
      subgraph = scope_node.subgraph_adapter
      out = {}
      exitvals = []
      topological_sort(subgraph.entry, subgraph).each { |n|
        inv = if subgraph.predecessors(n).empty?
                initial
              else
                predvals = subgraph.predecessors(n).map { |np| out[np] }
                TagCountDomain.join(predvals) { |t| blocks_for_tag(t) }
              end
        out[n] = count_tags_dfa(n, inv)
        exitvals.push(out[n]) if subgraph.successors(n).empty?
      }
      TagCountDomain.join(exitvals) { |t| blocks_for_tag(t) }
    when ScopeGraph::LoopNode
      get_all_tags(scope_node).inject(initial) { |tc,t|
        tc.access(t)
      }
    when ScopeGraph::BlockNode
      start = get_local_tags(scope_node).inject(initial) { |tc,t|
        tc.access(t)
      }
      scope_node.successors.inject(start) { |tc,callnode|
        count_tags_dfa(callnode, tc)
      }
    when ScopeGraph::CallSiteNode
      start = get_local_tags(scope_node).inject(initial) { |tc,t|
        tc.access(t)
      }
      scope_node.successors.inject(start) { |tc,fnode|
        count_tags_dfa(fnode, tc)
      }
    else
      assert("Unknown scopegraph node type: #{node.class}") { false }
    end
    r
  end

  # add additional cost to the IPET, specific to
  # a particular cache analysis
  def add_additional_cost(ipet_builder)
  end

  def add_access_edge(block, ctx, tag)
    assert("CacheRegionAnalysis#add_access_edge: tag is nil") { ! tag.nil? }
    block.outgoing_edges.each { |edge|
      ref = ContextRef.new(edge, ctx)
      access_edge = AccessEdge.new(ref, tag)
      access_edges.add(access_edge)
      access_edges_of_tag[tag] ||= Hash.new(0)
      access_edges_of_tag[tag][access_edge] += 1
    }
  end

  # add access variables to the IPET,
  def add_access_variables(ipet_builder)
    access_edges_of_edge = {}
    access_edges.each { |aedge|
      add_access_edge_variable(aedge, ipet_builder)
    }
  end

  def add_access_edge_variable(aedge, ipet_builder)
    debug(options, :cache) { "Adding access variable #{aedge}" }
    ipet_builder.ilp.add_variable(aedge)
    terms = Hash.new(0)
    terms[aedge]+=1
    ipet_builder.mc_model.edgeref_frequency(aedge.ppref.programpoint).each { |v,c|
      terms[v] -= c
    }
    c = ipet_builder.ilp.add_constraint(terms.to_a,"less-equal",0,"access_edge",cache_symbol)
    debug(options, :cache) { "Adding access edge constraint (relating to CFG) #{c}" }
  end

  # add constraints relating access variables
  # and tags (equality), and add cost to access variables
  def add_access_constraint(tag_var, tag_cost, ipet_builder)
    accesses = access_edges_of_tag[tag_var]
    debug(options, :cache) { "Accesses to tag #{tag_var}: #{accesses.inspect}" }
    terms = Hash.new(0)
    accesses.each { |access_variable, frequency|
      ipet_builder.ilp.add_cost(access_variable, frequency*tag_cost)
      terms[access_variable] += frequency
    }
    terms[tag_var] -= 1
    c = ipet_builder.ilp.add_constraint(terms.to_a,"equal",0,"tag_access",cache_symbol)
    debug(options, :cache) { "Adding access edge constraint (relating to memory) #{c}" }
  end


  def add_region_constraint(tag_var, bound, ipet_builder)
      terms = [[tag_var,1]]
      bound.each { |node, freq|
        if node.kind_of?(ScopeGraph::FunctionNode)
          terms += ipet_builder.mc_model.function_frequency(node.function,-freq)
        elsif node.kind_of?(ScopeGraph::LoopNode)
          terms += ipet_builder.mc_model.sum_loop_entry(node.loop, -freq)
        elsif node.kind_of?(ScopeGraph::BlockNode)
          terms += ipet_builder.mc_model.block_frequency(node.block, -freq)
        elsif node.kind_of?(ScopeGraph::CallSiteNode)
          terms += ipet_builder.mc_model.block_frequency(node.callsite.block, -freq)
        else
          raise Exception.new("Bad ScopeGraph node: #{node.class}")
        end
      }
      debug(options, :cache) { "Frequency bound of loading #{tag_var}: #{bound.inspect}" }
      ipet_builder.ilp.add_constraint(terms,"less-equal",0,"cache_miss_bound",cache_symbol)
  end

  # create an IPET variable representing a tag and return it
  #
  # NB: memory-transfer cost should only be associated with
  # (access) edges, not tags. Otherwise, creating a meaningful WCET profile
  # is difficult.
  def add_variable_for_tag(cache_tag, ipet_builder)
    ipet_builder.ilp.add_variable(cache_tag)
    cache_tag
  end

  #
  # methods to be implemented by subclasses
  #

  def miss_cost_for_tag(tag) ; raise Exception.new("#{self.class}: Not implemented: miss_cost_for_tag") ; end

  def record_local_tags(node) ; raise Exception.new("#{self.class}: Not implemented: record_local_tags") ; end

  def get_local_tags(node) ; raise Exception.new("#{self.class}: Not implemented: get_local_tags") ; end

  def tags_fit_in_cache?(tags) ; raise Exception.new("#{self.class}: Not implemented: tags_fit_in_cache") ; end

  def conflict_free_scope?(scope_node) ; raise Exception.new("#{self.class}: Not implemented: conflict_free_scope") ; end

  def cache_symbol ; raise Exception.new("#{self.class}: Not implemented: cache_symbol") ; end

end

# IPET variable that representes a tag access triggered by
# executing a certain CFG edge. Cache costs are always
# attributed to +AccessEdge+s, in order to facilitate
# the construction of WCET profiles.
class AccessEdge
  attr_reader :ppref, :tag
  include QNameObject
  def initialize(ppref, tag)
    @ppref, @tag = ppref, tag
    @qname = "#{ppref.qname}@#{tag.qname}"
  end
  def function
    @ppref.programpoint.function
  end
  def to_s
    "#{ppref}@#{tag}"
  end
end

#
# Method Cache Analysis. Tags are Subfunctions,
# that are accessed at the header block of the
# subfunction (cost mapped to its outgoing edges)
# and when returning to a function (cost mapped to
# the outgoing edges of the calling block).
#
class MethodCacheAnalysis < CacheRegionAnalysis

  def initialize(mc, pml, options)
    super(pml, options)
    @mc = mc
  end

  def analyze(scopegraph)
    scopegraph.assert_complete
    reset_analysis_results
    @block_subfunction_map = {}

    record_tags(scopegraph)
    collect_tag_constraints(scopegraph)
  end

  def cache_symbol
    :method_cache
  end

  def miss_cost_for_tag(subfunction)
    pml.arch.subfunction_miss_cost(subfunction)
  end


  # Coonceptually, method cache misses can be attributed to
  # entries of subfunction and callsites (miss-on-return).
  # We rely on the precondition that there is a one-block
  # region for every basic block in the scope graph.
  def record_local_tags(node)
    function = node.function
    unless sf_of_block = @block_subfunction_map[function]
      sf_of_block = @block_subfunction_map[function] = {}
      function.subfunctions.each { |sf|
        sf.blocks.each { |b| sf_of_block[b] = sf }
      }
    end
    case node
    when ScopeGraph::FunctionNode
    when ScopeGraph::LoopNode
    when ScopeGraph::BlockNode
      sf = sf_of_block[node.block]
      if node.block == sf.entry
        add_access_edge(node.block, node.context, sf)
      end
    when ScopeGraph::CallSiteNode
      sf = sf_of_block[node.callsite.block]
      add_access_edge(node.callsite.block, node.context, sf)
    else
      assert("Unknown scopegraph node type: #{node.class}") { false }
    end
  end

  # basically the same as +record_local_tags+
  def get_local_tags(node)
    function = node.function
    sf_of_block = @block_subfunction_map[function]
    case node
    when ScopeGraph::FunctionNode
      Set.new
    when ScopeGraph::LoopNode
      Set.new
    when ScopeGraph::BlockNode
      sf = sf_of_block[node.block]
      if node.block == sf.entry
        Set[sf]
      else
        Set.new
      end
    when ScopeGraph::CallSiteNode
      Set[sf_of_block[node.callsite.block]]
    else
      assert("Unknown scopegraph node type: #{node.class}") { false }
    end
  end

  def tags_fit_in_cache?(tags)
    tags.inject(0) { |sz, tag| sz + blocks_for_tag(tag) } <= @mc.associativity
  end

  def conflict_free_scope?(scope_node)
    return false if scope_node.kind_of?(ScopeGraph::LoopNode)
    tags = get_all_tags(scope_node)
    maximum_accessed_blocks = count_tags_dfa(scope_node, TagCountDomain.empty).total { |t| blocks_for_tag(t) }
    debug(options, :cache) {
      maximum_size = maximum_accessed_blocks * @mc.block_size
      total_size = tags.inject(0) { |sz, tag| sz + bytes_for_tag(tag) }
      "COMPARISON: total_size: #{total_size}, maximum_size: #{maximum_size}"
    }
    maximum_accessed_blocks <= @mc.associativity
  end

private
  def blocks_for_tag(subfunction)
    @mc.bytes_to_blocks(subfunction.size)
  end

  def bytes_for_tag(subfunction)
     blocks_for_tag(subfunction) * @mc.block_size
  end

end # class MethodCacheAnalysis

#
# Cache Region Analysis of set associative caches.
# The common property of these caches is tags are
# associated with exactly one cache set, and that
# there is no influence between cache sets (except
# for global replacement policies like pseudo-RR,
# which are not supported by us).
#
class SetAssociativeCacheAnalysis < CacheRegionAnalysis

  attr_reader :cache

  def initialize(cache, pml, options)
    super(pml, options)
    @cache = cache
    @offset_bits = Math.log2(cache.line_size).to_i
  end

  # the analysis of set associative caches
  # proceeds by analyzing conflicts for each set
  # separately
  def analyze(scope_graph)
    reset_analysis_results

    @set = nil # all sets
    record_tags(scope_graph)

    0.upto(num_sets - 1) { |set|
      @set = set
      collect_tag_constraints(scope_graph)
    }
  end

protected

  def associativity
    cache.associativity
  end

  def num_sets
    cache.size / (cache.associativity * cache.line_size)
  end

  def get_cache_lines(first_address, last_address)
    lines = []
    addr = get_cache_line(first_address)
    while addr <= last_address
      lines.push(addr)
      addr += cache.line_size
    end
    lines
  end

  def get_cache_line(addr)
    addr & ~(cache.line_size - 1)
  end

  def get_cache_set(addr)
    (addr >> @offset_bits) & (num_sets - 1)
  end

end

class InstructionCacheTag
  include QNameObject
  attr_reader :function
  attr_reader :addr
  # function is just for debugging purposes...
  def initialize(function, addrspace, addr)
    @function, @addr = function,addr
    @qname = sprintf("%s:0x%x", addrspace.to_s, addr)
  end
  def to_s
    @qname + "(#{@function})"
  end
end


class InstructionCacheAnalysis < SetAssociativeCacheAnalysis

  def initialize(ic, pml, options)
    super(ic, pml, options)
  end

  def record_local_tags(node)
    get_local_tags(node).each { |tag|
      if ScopeGraph::BlockNode === node
        add_access_edge(node.block, node.context, tag)
      elsif ScopeGraph::CallSiteNode === node
        add_access_edge(node.callsite.block, node.context, tag)
      else
        assert("SetAssociativeCacheAnalysis#record_local_tags: Not excepting tags (#{tag}) for region nodes") { false }
      end
    }
  end

  def get_local_tags(node)
    case node
    when ScopeGraph::FunctionNode
      Set.new
    when ScopeGraph::LoopNode
      Set.new
    when ScopeGraph::BlockNode
      # overapproximate: all cache lines in the current set possible loaded by the block
      return Set.new if node.block.instructions.empty?
      last_ins = node.block.instructions.last
      tags = get_cache_lines(node.block.address, last_ins.address + last_ins.size - 1).select { |line|
        @set.nil? || get_cache_set(line) == @set
      }.map { |line|
        InstructionCacheTag.new(node.block.function, cache_symbol, line)
      }
      Set[*tags]
    when ScopeGraph::CallSiteNode
      # might need to reload cache lines on return
      ins = node.callsite.call_return_instruction
      tags = get_cache_lines(ins.address, ins.address + ins.size - 1).select { |line|
        @set.nil? || get_cache_set(line) == @set
      }.map { |line|
        InstructionCacheTag.new(ins.function, cache_symbol, line)
      }
      Set[*tags]
    else
      assert("Unknown scopegraph node type: #{node.class}") { false }
    end
  end

  def miss_cost_for_tag(_)
    pml.arch.config.main_memory.read_delay(cache.line_size)
  end

  # XXX: remove me
  def tags_fit_in_cache?(tags)
    return false
  end

  def conflict_free_scope?(scope_node)
    tags = get_all_tags(scope_node)
    return true if tags.length <= associativity
    return false if scope_node.kind_of?(ScopeGraph::LoopNode)
    tags = get_all_tags(scope_node)
    maximum_accessed_lines = count_tags_dfa(scope_node, TagCountDomain.empty).total { |t| 1 }
    debug(options, :cache) {
      "COMPARISON: total_size: #{get_all_tags(scope_node).length}, maximum_size: #{maximum_accessed_lines}"
    }
    maximum_accessed_lines <= associativity
  end

  def cache_symbol
    :instruction_cache
  end
end

class DataCacheTag
  include QNameObject
  attr_reader :addr
  def initialize(addrspace, addr)
    @addrspace, @addr = addrspace,addr
    addrstr = addr.nil? ? '?' : sprintf("0x%x",addr)
    @qname = "#{addrspace}:#{addrstr}"
  end
  def unknown?
    addr.nil?
  end
  def function
    nil
  end
  def DataCacheTag.unknown(addrspace)
    DataCacheTag.new(addrspace, nil)
  end
  def to_s
    @qname
  end
end

# XXX: this does not belong here and is WILDLY prototypical
class IntervalSet
  def initialize
    @intervals = []
    @ranges = 0
  end
  def add!(range)
    @ranges += 1
    raise Exception.new("Bad range: #{range}") unless range.max
    # find the first interval (a,b), s.t. (r.min) <= b
    current = @intervals.dup
    newlist = []

    while(! current.empty? && current.first.max < range.min)
      newlist.push(current.shift)
    end
    if current.empty?
      # all intervals are before this one
      newlist.push(range)
    elsif range.max < current.first.min
      newlist.push(range)
      newlist.concat(current)
    else
      # we have range.min <= current.first.max
      # and     current.first.min <= range.max => mere
      newlist.push(merge_ranges(range,current.shift))
      newlist.concat(current)
    end
    @intervals = newlist
  end
  def intervals
    @intervals
  end
  def unknown?
    @ranges == 0
  end
  def size
    @intervals.map { |r| r.max - r.min + 1 }.inject(0) {|a,b|a+b}
  end
  def to_s
    @intervals.join(", ")
  end
end

class MemoryAccessAnalysisSummary
  def initialize(pml)
    @pml = pml
    @ins_info = {}
    build
  end
  def build
    @pml.valuefacts.each { |vf|
      next unless vf.ppref.programpoint.kind_of?(Instruction)
      # XXX: we need to check whether we have information for ALL contexts, really
      info = (@ins_info[vf.programpoint] ||= IntervalSet.new)
      vf.values.each_with_index { |v,ix|
        info.add!(v.range) if v.range
      }
    }
    # @ins_info.each { |i,s|
    #  puts "#{i}: #{s}"
    # }
  end
  def get_interval_sets(ins)
    @ins_info[ins]
  end
end

class AccessInstruction
  include QNameObject
  attr_reader :ins, :tag
  def initialize(ins, tag)
    @ins, @tag = ins, tag
    @qname = "#{ins}@#{tag}"
  end
  def function
    @ins.function
  end
  def to_s
    @qname
  end
end

class DataCacheAnalysis < SetAssociativeCacheAnalysis

  def initialize(dc, pml, options)
    super(dc, pml, options)
  end

  def reset_analysis_results
    super()
    # summary of value analysis results
    @value_analysis = MemoryAccessAnalysisSummary.new(pml)
    # list of instruction that access the data cache
    @access_instructions = []
    # map from tag to instructions that might access it
    @access_points = {}
    # bypass stores and loads
    @bypass_stores = Hash.new(0)
    @bypass_loads = Hash.new(0)
  end


  def record_local_tags(node)
    if ScopeGraph::BlockNode === node
      node.block.instructions.each { |ins|
        if ins.memtype == 'cache'
          if ins.load_mem?
            @access_instructions.push(ins)
            get_instruction_tags(ins).each { |tag|
              (@access_points[tag]||=[]).push(ins)
            }
          else
            # assuming non-allocating write-through
            @bypass_stores[ins.block] += 1
          end
        elsif ins.memtype == 'memory'
          if ins.load_mem?
            @bypass_loads[ins.block] += 1
          else
            @bypass_stores[ins.block] += 1
          end
        end
      }
    end
  end

  def get_local_tags(node)
    case node
    when ScopeGraph::FunctionNode
      Set.new
    when ScopeGraph::LoopNode
      Set.new
    when ScopeGraph::CallSiteNode
      Set.new
    when ScopeGraph::BlockNode
      # overapproximate: all cache lines in the current set possible loaded by the block
      return Set.new if node.block.instructions.empty?
      tags = node.block.instructions.map { |ins|
        get_instruction_tags(ins, @set)
      }.flatten
      Set[*tags]
    else
      assert("Unknown scopegraph node type: #{node.class}") { false }
    end
  end

  # add miss cost for stores
  def add_additional_cost(ipet_builder)
    @bypass_stores.each { |block,count|
      ipet_builder.mc_model.add_block_cost(block, cost_for_line_store * count)
    }
    @bypass_loads.each { |block,count|
      ipet_builder.mc_model.add_block_cost(block, cost_for_bypass_load * count)
    }
  end

  # add constraints for access instructions
  def add_access_variables(ipet_builder)
    @access_instructions.each { |ins|
      # An access instruction is an instruction that potentially accesses on
      # out of N D$ tags. In order to map this to edges, we split each
      # (ins,tag) pair into M (edge, ins, tag) triples, for every outgoing
      # egde of the block the instruction is in.
      #
      # We thus combine
      #  <tt>sum_{t in tags(in)} f(ins@tag) <= f(ins)</tt>
      # and
      #  <tt>f(ins@tag) = sum_{e in outgoing(ins.block)} f(e@ins@tag)</tt>
      # where costs are associated with access edges (e@ins->tag)
      #
      ipet_builder.mc_model.add_instruction(ins)
      vins = Set[*get_instruction_tags(ins)].map { |tag|
        AccessInstruction.new(ins, tag)
      }
      vi_terms = Hash.new(0)
      vi_terms[ins] = -1
      vins.each { |vi|
        debug(options, :cache) { "Adding access instruction #{vi.inspect}" }
        ipet_builder.ilp.add_variable(vi)

        # we need to attach cost to edges -> another level of indirection in the ILP
        edge_terms = Hash.new(0)
        edge_terms[vi] = -1
        miss_cost = miss_cost_for_tag(vi.tag)

        ins.block.outgoing_edges.each { |edge|
          ref = ContextRef.new(edge, Context.empty)
          access_edge = AccessEdge.new(ref, vi)
          debug(options, :cache) { "Adding access edge #{access_edge} -> #{miss_cost}" }
          add_access_edge_variable(access_edge, ipet_builder)
          ipet_builder.ilp.add_cost(access_edge, miss_cost)
          edge_terms[access_edge] += 1
        }
        ipet_builder.ilp.add_constraint(edge_terms.to_a,"equal",0,"access_instruction_edges",cache_symbol)
        vi_terms[vi] = 1
      }
      ipet_builder.ilp.add_constraint(vi_terms.to_a,"less-equal",0,"access_instruction",cache_symbol)
    }
  end

  # To kind of constraints:
  # +tag-access+:: A tag +t+ accesses access instructions <tt>{ i/t | i in accessors(t) }</tt>
  # +access-ins+:: The frequency of a set of access instructions <tt> { i/t | t in tags(i) }</tt>
  #                is equal to the frequency of i (see add_additional_constraints)
  def add_access_constraint(tag_var, _tag_cost, ipet_builder)
    # Tag Access
    terms = {}
    terms[tag_var] = -1
    @access_points[tag_var].each { |i|
      # access instructions
      ai = AccessInstruction.new(i, tag_var)
      terms[ai] = 1
    }
    debug(options, :cache) { "Access points for #{tag_var}: #{@access_points[tag_var].inspect}" }
    ipet_builder.ilp.add_constraint(terms.to_a,"less-equal",0,"known_tag_load",cache_symbol)
  end

  def miss_cost_for_tag(tag)
    pml.arch.config.main_memory.read_delay(cache.line_size)
  end

  def cost_for_bypass_load
    pml.arch.config.main_memory.read_delay(cache.line_size)
  end

  def cost_for_line_store
    pml.arch.config.main_memory.write_delay(cache.line_size)
  end

  def tags_fit_in_cache?(tags)
    return false if tags.any? { |tag| tag.unknown? }
    tags.size <= associativity
  end

  def conflict_free_scope?(scope_node)
    false # no precise implementation
  end

  def cache_symbol
    :data_cache
  end

private
  def get_instruction_tags(ins, set = nil)
    if ins.memtype == 'cache' && ins.load_mem?
      isets = @value_analysis.get_interval_sets(ins)
      if ! isets || isets.unknown?
        [DataCacheTag.unknown(cache_symbol)]
      elsif isets.size > 1024*1024 # cache.size
        [DataCacheTag.unknown(cache_symbol)]
      else
        isets.intervals.map { |range|
          tags = get_cache_lines(range.min, range.max).select { |line|
            set.nil? || get_cache_set(line) == set
          }.map { |line|
            DataCacheTag.new(cache_symbol,line)
          }
        }.flatten
      end
    else
      []
    end
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

class TagCountDomain
  attr_reader :tags, :additional
  def initialize(tags, additional)
    @tags, @additional = tags, additional
  end
  def total
    #puts "SUMMARIZING COST"
    @tags.inject(@additional) { |v,t|
      cost_of_t = yield t
      #puts "TAG #{t} with cost #{cost_of_t}"
      v + cost_of_t
    }
  end
  def TagCountDomain.empty
    TagCountDomain.new(Set.new, 0)
  end
  def access(tag)
    @tags.add(tag)
    r = TagCountDomain.new(tags + Set[tag], additional)
    r
  end
  def to_s
    "#<TagCountDomain tags=#{tags.inspect} additional=#{additional}>"
  end
  def TagCountDomain.join(vs)
    return TagCountDomain.empty if vs.empty?
    joined = vs.first
    vs[1..-1].each { |v2|
      joined = TagCountDomain.join2(joined, v2) { |t| yield t }
    }
    joined
  end
  def TagCountDomain.join2(v1,v2)
    common = v1.tags.intersection(v2.tags)
    v1_only = v1.tags - common
    v2_only = v2.tags - common
    new_tags = common
    new_additional = [v1,v2].map { |v| v.additional }.max
    if v1_only.empty? || v2_only.empty?
      new_tags = v1.tags + v2.tags
    else
      new_additional += [v1_only,v2_only].map { |t| yield t }.inject(0) { |a,b| a+b }
      #puts "JOIN results in #{v1} \/ #{v2} ADDITIONAL #{new_additional}"
    end
    r = TagCountDomain.new(new_tags, new_additional)
    r
  end
end

end # module PML
