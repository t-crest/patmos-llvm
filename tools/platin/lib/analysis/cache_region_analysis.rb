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

  # This function computes +@load_tag_constraints+, that assigns
  # cache tags a linear expression over +ScopeGraph+ nodes.
  # For example,
  #  <tt>load_tag_constraints[X] = { F:compute => 2, L:main/3 => 1 }</tt>
  # means that the number of times X is loaded into the cache is
  # bounded by the frequency of function +compute+ times 2 plus the
  # frequency of the loop entry edges of the loop <tt>main/3+</tt>.
  def analyze(scopegraph)
    @load_tag_constraints = {}
    collect_tag_constraints(scopegraph)
  end

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
        debug(options, :cache) { "Conflicts in Scope: #{node} #{@set}" }

        # and account for the miss cost of all
        # conflict-free subscopes
        analyze_conflict_scope(node)
      end
    }
  end

  # Modifies the IPET problem as follows:
  #   (1) create a variable for the frequency of loads of a tag
  #   (2) bound the loads by the execution frequency of the
  #       program points (e.g., blocks,instructions or virtual instructions)
  #        accessing the tag
  #   (3) bound the execution frequency of the load using
  #       the information on conflict-free regions
  def add_miss_cost(ipet_builder)
    add_additional_constraints(ipet_builder)
    @load_tag_constraints.each { |tag, bound|
      tag_var = add_variable_for_tag(tag, ipet_builder)
      miss_cost = miss_cost_for_tag(tag)
      debug(options, :cache) { "Cost for loading #{tag}: #{miss_cost} (#{size_of_tag(tag)})" }
      ipet_builder.ilp.add_cost(tag_var, miss_cost)

      add_access_constraint(tag_var, ipet_builder)

      add_region_constraint(tag_var, bound, ipet_builder)
    }
  end

  # add additional constraints or variables to the IPET,
  # specific to the particular cache analysis
  def add_additional_constraints(ipet_builder)
  end

protected

  def analyze_conflict_scope(node)

    debug(options, :cache) { "Conflict in scope #{node}: #{get_all_tags(node).inspect}" }

    # We might need to load each (only one for method cache) locally
    # accessed tag once per execution of the node ...
    get_local_tags(node).each { |t|
      increment_load_tag_bound(t, node)
    }

    # no subscopes
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
           # debug(options, :cache) { |&iop| iop.call("TAGS for scope #{scope_node} (#{@set})");tags.each { |tag| iop.call("TAG: #{tag}") } }
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

  # overwrite for data cache
  def add_access_constraint(tag_var, ipet_builder)
    terms = Hash.new(0)
    terms[tag_var] += 1
    access_blocks(tag_var).each { |b,f|
      ipet_builder.mc_model.block_frequency(b, -1 * f).each { |k,v|
        terms[k] += v
      }
    }
    ipet_builder.ilp.add_constraint(terms.to_a,"less-equal",0,"cache_load",cache_symbol)
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

end

class MethodCacheAnalysis < CacheRegionAnalysis

  def initialize(mc, pml, options)
    super(pml, options)
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
    return false if scope_node.kind_of?(ScopeGraph::LoopNode)
    tags = get_all_tags(scope_node)
    maximum_accessed_blocks = count_tags_dfa(scope_node, TagCountDomain.empty).total { |t| blocks_for_tag(t) }
    debug(options, :cache) {
      maximum_size = maximum_accessed_blocks * block_size
      total_size = tags.inject(0) { |sz, tag| sz + bytes_for_tag(tag) }
      "COMPARISON: total_size: #{total_size}, maximum_size: #{maximum_size}"
    }
    maximum_accessed_blocks <= associativity
  end

  def size_of_tag(subfunction)
    subfunction.size
  end
end

class SetAssociativeCacheAnalysis < CacheRegionAnalysis
  attr_reader :cache
  def initialize(cache, pml, options)
    super(pml, options)
    @cache = cache
    @offset_bits = Math.log2(cache.line_size).to_i
  end

  def analyze(scope_graph)
    @load_tag_constraints = {}
    @access_points = {}
    @set = nil # all sets
    scope_graph.bottom_up.each { |n|
      record_local_tags(n)
    }
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

  def add_variable_for_tag(cache_tag, ipet_builder)
    ipet_builder.ilp.add_variable(cache_tag)
    cache_tag
  end

  def size_of_tag(cache_tag)
    cache.line_size
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
        @access_points[tag] ||= Hash.new(0)
        @access_points[tag][node.block] += 1
      elsif ScopeGraph::CallSiteNode === node
        @access_points[tag] ||= Hash.new(0)
        @access_points[tag][node.callsite.block] += 1
      else
        assert("SetAssociativeCacheAnalysis#record_local_tags: Not excepting tags (#{tag}) for region nodes") { false }
      end
    }
  end

  def access_blocks(cache_tag)
    @access_points[cache_tag]
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
        InstructionCacheTag.new(node.block.function,cache_symbol,line)
      }
      Set[*tags]
    when ScopeGraph::CallSiteNode
      # might need to reload cache lines on return
      ins = node.callsite
      tags = get_cache_lines(ins.address, ins.address + ins.size - 1).select { |line|
        @set.nil? || get_cache_set(line) == @set
      }.map { |line|
        InstructionCacheTag.new(ins.function,cache_symbol,line)
      }
      Set[*tags]
    else
      assert("Unknown scopegraph node type: #{node.class}") { false }
    end
  end

  def miss_cost_for_tag(_)
    pml.arch.config.main_memory.read_delay(cache.line_size)
  end

  def tags_fit_in_cache?(tags)
    tags.length <= associativity
  end

  def tags_fit_in_cache_precise?(scope_node)
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
  def initialize(ins, tag)
    @ins, @tag = ins, tag
    @qname = "#{ins}->#{tag}"
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
    @access_instructions = []
    @bypass_stores = Hash.new(0)
    @bypass_loads = Hash.new(0)
    @value_analysis = MemoryAccessAnalysisSummary.new(pml)
  end

  # add miss cost for stores
  def add_additional_constraints(ipet_builder)
    @bypass_stores.each { |block,count|
      ipet_builder.mc_model.add_block_cost(block, cost_for_line_store * count)
    }
    @bypass_loads.each { |block,count|
      ipet_builder.mc_model.add_block_cost(block, cost_for_bypass_load * count)
    }
    # Access Instructions
    debug(options, :cache) { "Adding access instruction constraints" }
    @access_instructions.each { |ins|
      ipet_builder.mc_model.add_instruction(ins)
      vins = Set[*get_instruction_tags(ins)].map { |tag|
        AccessInstruction.new(ins, tag)
      }
      vi_terms = Hash.new(0)
      vi_terms[ins] = -1
      vins.each { |vi|
        debug(options, :cache) { "Adding access instruction #{vi.inspect}" }
        ipet_builder.ilp.add_variable(vi)
        vi_terms[vi] = 1
      }
      ipet_builder.ilp.add_constraint(vi_terms.to_a,"less-equal",0,"access_instruction",cache_symbol)
    }
    debug(options, :cache) { "Adding access instruction constraints (done)" }
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

  # To kind of constraints:
  # +tag-access+:: A tag +t+ accesses access instructions <tt>{ i/t | i in accessors(t) }</tt>
  # +access-ins+:: The frequency of a set of access instructions <tt> { i/t | t in tags(i) }</tt>
  #                is equal to the frequency of i (see add_additional_constraints)
  def add_access_constraint(tag_var, ipet_builder)
    # Tag Access
    terms = {}
    terms[tag_var] = 1
    @access_points[tag_var].each { |i|
      # access instructions
      ai = AccessInstruction.new(i, tag_var)
      terms[ai] = -1
    }
    debug(options, :cache) { "Access points for #{tag_var}: #{@access_points[tag_var].inspect}" }
    ipet_builder.ilp.add_constraint(terms.to_a,"less-equal",0,"known_tag_load",cache_symbol)
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

  def tags_fit_in_cache_precise?(scope_node)
    false # no precise implementation
  end

  def cache_symbol
    :data_cache
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
