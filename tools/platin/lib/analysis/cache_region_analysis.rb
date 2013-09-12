#
# platin tool set
#
# == Cache Persistence Analysis via Conflict-Free Region Identification
#
# - Each instruction I is associated with one or more tags it *might* access
#   In the following, we instrument the program to illustrate the semantics
#   we try to capture. First, we introduce variables LoadTag[T] for every
#   tag T. The cache access is modeled by nondeterminstic choice:
#
#     switch(?) 0:   LoadTag[I.getTag(0)]++;
#               ...
#               N:   LoadTag[I.getTag(1)]++;
#               N+1: skip; // cache hit
#
# - Each tag maps to a cache set CS. We write Tags(S, CS) to denote the set of
#   tags that map to CS and are possibly accessed during the execution of S,
#   and Sets(S) to denote the set of cache sets possibly accessed when executing S.
#
# - A cache set CS of associativity N is conflict-free with respect to a scope S,
#   if it is guarantueed that at most N *distinct* tags mapping to CS are accessed
#   during the execution of S (assuming LRU or FIFO replacement)
#
# - If CS is conflict free in S, each tag is loaded as most once. The (virtual)
#   instrumentation modeling this fact is
#
#     Entry(S):
#        for CS in Sets(S):
#            for T in Tags(S, CS):
#               LoadCountEntry(S,T) = LoadTag[T]
#     Exit(S):
#        for CS in Sets(S):
#            for T in Tags(S, CS):
#               assert(LoadTag[T] - LoadCountEntry(S,T) <= 1)
#
# - For every cache set CS and scope S there is a set of scopes SS,
#   called the "conflict-free scope subgraph for CS containing S", that satisfies:
#   (a) Every path from the scope graph's root node to S passes through one of
#       the scopes S' in SS
#   (b) The scope in SS are unrelated (i.e., there is no path from one scope S' in SS
#       to another scope S'' in SS in the scope graph)
#   (c) Every scope S' in SS is conflict-free with respect to CS
#
# - For every cache set CS there is a set of *conflict free regions* CFRs(CS),
#   scope subgraphs CFR that are conflict-free with respect to CS, such that every
#   access to CS is contained in one of the scope subgraphs.
#   We write Access(CFR, Tag) if there is a potential access to Tag in a CFR.
#   We color the scope graph bottom-up for S
#   - If CS is conflict-free with respect to S, set S.conflictFree(CS) = true
#   - Otherwise, for each outgoing edge of S->S' in the scope graph, if
#     S'.conflictFree(CS) and S'.mayAccess(CS), add (S->S') to CFRs(CS).
#
# - In the IPET realization of this model, we (elaborate)
#
#   (1) add a variable LoadTag(I,T) for every tag T possible accesed by instruction I
#       with
#         sum_{T accessed by I}       f(LoadTag(I,T)) <= f(I)
#
#       Special case: address range is huge
#
#   (2) For all S->S', the entry to a conflict free region of CS, add
#
#         sum_{T accessed by I \in SS, Set(T) = CS} f(LoadTag(I,T)) <= sum_{E = Enf(S->S')
#
# - Method Cache Example (8 blocks)
#
# m: 1, f:4, g:3, h:2
# m: loop { 1: f(); 2: g(); }; 3: h(); loop { 4: f() };
# f: loop { 1: h() };
# g,h: {}
#
# Scope Graph
# -----------
# m -> M.L1 -> m.2 -> g
#           -> m.1 -> f -> f.L1 -> f.1 -> h
#   -> M.L2 -> m.4 ---^                   ^
#   -- m.3 -------------------------------/
#
# Conflicts
# ---------
# h:    {h}        2 blocks
# f.L1: {f,h}      6 blocks
# f.1:  {f,h}      6 blocks
# f:    {f,h}      6 blocks
# g:    {g}        3 blocks
# m.1   {m,f,h}    7 blocks
# m.2   {m,g}      4 blocks
# m.3   {m,h}      3 blocks
# m.L1  {m,f,g,h} 10 blocks (conflicting)
# m.4   {m,f,h}    7 blocks
# m.L2  {m,f,h}    7 blocks
# m     {m,f,g,h} 10 blocks (conflicting)
#
# Conflict Free Regions
# ---------------------
#
# m.L1->m.1 {m,f,h}  [m1,f1]
# m.L1->m.2 {m,g}    [m2]
# m->m.3    {m,h}    [m3]
# m->m.L2   {m,f,h}  [m4,f1]
#
# Miss Variables and Constraints
# ------------------------------
# loadm, loadf, loadg, loadh
# loadm <= f(m) + f(m.1) + f(m.2) + f(m.3) + f(m.L2)
# loadf <- f(m.1) + f(m.L2)
# loadg <= f(m.2)
# loadh <= f(m.1) + f(m.L2)

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
      mca.add_miss_cost(ipet_builder)
    elsif ic = @pml.arch.instruction_cache
      ica = InstructionCacheAnalysis.new(ic, @pml, @options)
      ica.analyze(scope_graph)
      ica.add_miss_cost(ipet_builder)
    end
    if sc = @pml.arch.stack_cache
      sca = StackCacheAnalysis.new(sc, @pml, @options)
      sca.analyze(scope_graph)
      sca.add_miss_cost(ipet_builder)
    end
    if dc = @pml.arch.data_cache
      dca = DataCacheAnalysis.new(dc, @pml, @options)
      dca.analyze(scope_graph)
      dca.add_miss_cost(ipet_builder)
    end
  end
end

class CacheRegionAnalysis
  attr_reader :pml, :options

  def initialize(pml, options)
    @pml, @options = pml, options
  end

  def analyze(scopegraph)
    @load_tag_constraints = {}
    collect_tag_constraints(scopegraph)
  end

  # Analyze computes +load_tag_constraints+, that map
  # tags to a +Hash+ from +ScopeGraph+ nodes to coefficients
  # For example,
  #  <tt>load_tag_constraints[X] = { F:compute => 2, L:main/3 => 1 }</tt>
  # means that the number of times X is loaded into the cache is
  # bounded by the frequency of function +compute+ times 2 plus the
  # frequency of the loop entry edges of the loop +main/3+.
  def collect_tag_constraints(scopegraph)
    @all_tags = {}
    @conflict_free = {}
    scopegraph.bottom_up.each { |node|
      if conflict_free?(node)
        # if the node is conflict free, just mark it (unless it is the entry)
        debug(options, :cache) { "Conflict-free: #{node}" }
        if node == scopegraph.root
          @all_tags[node].each { |t|
            increment_load_tag_bound(t,node)
          }
        end
      else
        debug(options, :cache) { "Conflicts in: #{node}" }
        # and account for the miss cost of all
        # conflict-free subscopes
        analyze_conflict_scope(node)
      end
    }
  end

  # (1) create a variable for each load of a tag
  # (2) bound the loads by the execution frequency of the
  #     blocks accessing the tag
  # (3) bound the execution frequency of the load using
  #     the information on conflict-free regions
  def add_miss_cost(ipet_builder)
    @load_tag_constraints.each { |tag, bound|
      tag_var = add_variable_for_tag(tag, ipet_builder)
      miss_cost = miss_cost_for_tag(tag)
      debug(options, :cache) { "Cost for loading #{tag}: #{miss_cost} (#{tag.size})" }
      debug(options, :cache) { "Frequency bound of loading #{tag}: #{bound.inspect}" }
      ipet_builder.ilp.add_cost(tag_var, miss_cost)
      terms = Hash.new(0)
      terms[tag_var] += 1
      access_blocks(tag).each { |b,f|
        ipet_builder.mc_model.block_frequency(b, -1 * f).each { |k,v|
          terms[k] += v
        }
      }
      c1 = ipet_builder.ilp.add_constraint(terms.to_a,"less-equal",0,"cache_load",cache_symbol)

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
      c2 = ipet_builder.ilp.add_constraint(terms,"less-equal",0,"cache_miss_bound",cache_symbol)
    }
  end

protected

  # Our progress on a benchmark that performs bad:
  #  fly_by_wire-main-blockglobal, Method Cache
  #    - all miss in conflict:       142957
  #    - group callsites in blocks:  135157
  #    - group regions in functions: 132405
  #    - group regions in f/l:       132589
  #    - measured                     99738
  def analyze_conflict_scope(node)

    # We might need to load each (only one for method cache) locally
    # accessed tag once per execution of the node ...
    get_local_tags(node).each { |t|
      increment_load_tag_bound(t, node)
    }
    return if node.successors.empty?
    # We want to form large conflict-free subregions of region scopes and block scopes
    # Here we use a simple greedy heuristic
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
           debug(options, :cache) { |&iop| iop.call("TAGS for scope #{scope_node} (#{@set})");tags.each { |tag| iop.call("TAG: #{tag}") } }
           if tags_fit_in_cache?(tags)
             true
           elsif tags_fit_in_cache_precise?(scope_node)
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
    debug(options,:cache) { "Execution of #{tag} is bounded by #{node}" }
    (@load_tag_constraints[tag]||=Hash.new(0))[node] += 1
  end
end

class MethodCacheAnalysis < CacheRegionAnalysis

  def initialize(mc, pml, options)
    super(@pml, @options)
    @mc = mc
  end

  def analyze(scopegraph)
    scopegraph.assert_complete
    @load_tag_constraints = {}
    collect_tag_constraints(scopegraph)
  end

  def cache_symbol
    :method_cache
  end

  def add_variable_for_tag(subfunction, ipet_builder)
    ipet_builder.ilp.add_variable(subfunction)
    subfunction
  end

  def miss_cost_for_tag(subfunction)
    pml.arch.subfunction_miss_cost(subfunction)
  end


  def access_blocks(subfunction)
    accesses = Hash.new(0)
    accesses[subfunction.entry] = 1
    subfunction.blocks.each { |b|
      b.callsites.each { |cs|
        accesses[cs.block] += 1
      }
    }
    accesses
  end

  def block_size
    pml.arch.method_cache.block_size
  end

  def cache_size
    pml.arch.method_cache.size
  end

  def associativity
    assert("Bad method cache associativity") { pml.arch.method_cache.associativity == cache_size / block_size }
    pml.arch.method_cache.associativity
  end

  def blocks_for_tag(subfunction)
    pml.arch.method_cache.bytes_to_blocks(subfunction.size)
  end

  def bytes_for_tag(subfunction)
     blocks_for_tag(subfunction) * block_size
  end

  # conceptually, the method cache is accessed at the subfunction
  # entry block, and at callsites
  # we assume there is a one-block region for every basic block
  # in the scope graph
  def get_local_tags(node)
    function = node.function
    @block_subfunction_map ||= {}
    unless sf_of_block = @block_subfunction_map[function]
      sf_of_block = @block_subfunction_map[function] = {}
      function.subfunctions.each { |sf|
        sf.blocks.each { |b| sf_of_block[b] = sf }
      }
    end
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
    tags.inject(0) { |sz, tag| sz + blocks_for_tag(tag) } <= associativity
  end

  def tags_fit_in_cache_precise?(scope_node)
    return false unless scope_node.kind_of?(ScopeGraph::LoopNode)
    tags = get_all_tags(scope_node)
    maximum_accessed_blocks = count_tags_dfa(scope_node, TagCountDomain.empty).total { |t| blocks_for_tag(t) }
    debug(options, :cache) {
      maximum_size = maximum_accessed_blocks * block_size
      total_size = tags.inject(0) { |sz, tag| sz + bytes_for_tag(tag) }
      "COMPARISON: total_size: #{total_size}, maximum_size: #{maximum_size}"
    }
    maximum_accessed_blocks <= associativity
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


end

class CacheTag
  include QNameObject
  attr_reader :function
  attr_reader :addr
  # function is just for debugging purposes...
  def initialize(function, addrspace, addr)
    @function, @addr = function,addr
    @qname = sprintf("%s:0x%x", addrspace.to_s, addr)
  end
  def size
    32
  end
  def to_s
    @qname + "(#{@function})"
  end
end

class SetAssociativeCacheAnalysis < CacheRegionAnalysis
  attr_reader :cache
  def initialize(cache, pml, options)
    super(pml, options)
    @cache = cache
    @offset_bits = Math.log2(cache.line_size).to_i
  end
protected
  def tags_fit_in_cache?(tags)
    tags.length <= associativity
  end
  def tags_fit_in_cache_precise?(scope_node)
    false
  end
  def associativity
    cache.associativity
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
    (addr >> @offset_bits) & (cache.associativity - 1)
  end
end

class InstructionCacheAnalysis < SetAssociativeCacheAnalysis
  def initialize(ic, pml, options)
    super(ic, pml, options)
    @access_blocks = {}
  end
  def analyze(scope_graph)
    @load_tag_constraints = {}
    0.upto(associativity-1) { |set|
      @set = set
      collect_tag_constraints(scope_graph)
    }
  end

  def add_variable_for_tag(cache_tag, ipet_builder)
    ipet_builder.ilp.add_variable(cache_tag)
    cache_tag
  end

  def miss_cost_for_tag(_)
    pml.arch.config.main_memory.read_delay(cache.line_size)
  end

  def access_blocks(cache_tag)
    @access_blocks[cache_tag]
  end

  def add_access(cache_tag, block)
    @access_blocks[cache_tag] ||= Hash.new(0)
    @access_blocks[cache_tag][block] += 1
  end

  # FIXME: this function should not have side effects !!!
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
      lines = get_cache_lines(node.block.address, last_ins.address + last_ins.size - 1).select { |line|
        get_cache_set(line) == @set
      }
      tags = lines.map { |line|
        tag = CacheTag.new(node.block.function,cache_symbol,line)
        add_access(tag, node.block)
        tag
      }
      Set[*tags]
    when ScopeGraph::CallSiteNode
      # might need to reload cache lines on return
      ins = node.callsite
      lines = get_cache_lines(ins.address, ins.address + ins.size - 1).select { |line|
        get_cache_set(line) == @set
      }
      tags = lines.map { |line|
        tag = CacheTag.new(ins.function,cache_symbol,line)
        add_access(tag, ins.block)
        tag
      }
      Set[*tags]
    else
      assert("Unknown scopegraph node type: #{node.class}") { false }
    end
  end
  def cache_symbol
    :instruction_cache
  end
end

class DataCacheAnalysis < CacheRegionAnalysis
  def initialize(dc, pml, options)
    raise Exception.new("Cache Region Analysis for data memory accesses not yet implemented")
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
  def add_miss_cost(ipet_builder)
    ilp = ipet_builder.ilp
    @fill_blocks.each { |instruction,fill_blocks|
      ipet_builder.mc_model.add_instruction(instruction)
      ilp.add_cost(instruction, @pml.arch.config.main_memory.read_delay(fill_blocks*@cache.block_size))
    }
    @spill_blocks.each { |instruction,spill_blocks|
      ipet_builder.mc_model.add_instruction(instruction)
      ilp.add_cost(instruction, @pml.arch.config.main_memory.write_delay(spill_blocks*@cache.block_size))
    }
  end
end

class TagCountDomain
  attr_reader :tags, :additional
  def initialize(tags, additional)
    @tags, @additional = tags, additional
  end
  def total
    puts "SUMMARIZING COST"
    @tags.inject(@additional) { |v,t|
      cost_of_t = yield t
      puts "TAG #{t} with blocks #{cost_of_t} / #{t.size} bytes"
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
      puts "JOIN results in #{v1} \/ #{v2} ADDITIONAL #{new_additional}"
    end
    r = TagCountDomain.new(new_tags, new_additional)
    r
  end
end

end # module PML
