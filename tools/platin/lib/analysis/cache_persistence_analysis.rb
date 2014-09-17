#
# platin tool set
#
# == Scope-Based Cache Analysis: LRU persistence
#
# See Article [TODO]

require 'set'

#
# local persistence analysis
# Inspired by the global analysis of [Hyunh et.al 2012]
#
class PersistenceDataFlowAnalysis

  # for each cache tag T possibly accessed in the scope, compute
  #
  #  NO set:   other tags possibly accessed on paths where T is not accessed at all
  #  IN set:   other tags possibly accessed up to the first access of T
  #  OUT set:  other tags possibly accessed from the last acces to T on
  #
  #

  # forward declaration
  class TagSet
  end

  class ZeroTagSet < TagSet
    def access(t); self;     end
    def concat(set); self ; end
    def join(other); other ; end
    def to_s; "0"; end
    def dup ; self ; end
  end

  class TopTagSet < TagSet
    def access(t);   self ; end
    def concat(set); (set == ZERO) ? ZERO : self; end
    def join(other); self ; end
    def to_s; "1"; end
    def dup; self ; end
  end

  class TagSet
    attr_reader :set
    ZERO = ZeroTagSet.new
    TOP  = TopTagSet.new
    def initialize(initial_set, check_conflict)
      @set = initial_set
      @check_conflict = check_conflict
    end
    def create(set)
      return TOP if @check_conflict.call(set)
      TagSet.new(set, @check_conflict)
    end
    def access(t)
      if self == ZERO
        self
      elsif self == TOP
        self
      else
        create(@set + Set[t])
      end
    end
    def concat(set)
      if set == ZERO || set == TOP
        set.concat(self)
      else
        create(@set + set.set)
      end
    end
    def join(other)
      return other.join(self) if  other == ZERO || other == TOP
      create(@set + other.set)
    end
    def dup
      TagSet.new(@set.dup, @check_conflict)
    end
    def to_s
      "{#{@set.to_a.join(",")}}"
    end
    def ==(other)
      return (self.equal?(ZERO)) if other.equal?(ZERO)
      return (self.equal?(TOP)) if other.equal?(TOP)
      return @set == other.set
    end
  end

  class CacheSetMap
    def initialize(active_tags, check_conflict)
      @active_tags, @check_conflict = active_tags, check_conflict
      @no_map, @in_map, @out_map = {}, {}, {}
      @active_tags.each { |t|
        @no_map[t]  = new_tag_set(t)
        @in_map[t]  = TagSet::ZERO
        @out_map[t] = TagSet::ZERO
      }
    end

    # if t is not active in this scope, any of the active tags might
    # have been accessed without accessing t
    def no(t) ; @no_map[t] || new_tag_set(t, @active_tags); end
    def in(t) ; @in_map[t] || TagSet::ZERO; end
    def out(t) ; @out_map[t] || TagSet::ZERO; end

    def new_tag_set(t, initial_set = nil)
      t_metric = Proc.new { |set|
        @check_conflict.call(set + Set[t])
        # info("Conflicting?(#{t} -> #{set.to_a})=#{r}")
      }
      TagSet.new(initial_set || Set.new, t_metric)
    end
    def access(x)
      @active_tags.each { |t|
        old_no_map = @no_map[t]
        @no_map[t] = if t == x
                       TagSet::ZERO
                     else
                       @no_map[t].access(x)
                     end
        @in_map[t] = if t == x
                       @in_map[t].join(old_no_map)
                     else
                       @in_map[t]
                     end
        @out_map[t] = if t == x
                        new_tag_set(t)
                      else
                        @out_map[t].access(x)
                      end
      }
      self
    end

    def join_with(*others)
      changed = false
      @active_tags.each { |t|
        [[@no_map,:no],[@in_map,:in],[@out_map,:out]].each { |m,sym|
          inval = m[t]
          joinval = inval
          others.each { |other|
            joinval = joinval.join(other.send(sym,t))
          }
          if inval != joinval
            changed = true
            m[t] = joinval
          end
        }
      }
      [self, changed]
    end

    def call(results)
      @active_tags.each { |t|
        @in_map[t] = @in_map[t].join(@no_map[t].concat(results.in(t)))
        @no_map[t] = @no_map[t].concat(results.no(t))
        @out_map[t] = @out_map[t].concat(results.no(t)).join(results.out(t))
      }
      self
    end

    def dup
      CacheSetMap.new(@active_tags, @check_conflict).copy_from(self)
    end

    def copy_from(other)
      @active_tags.each { |t|
        @no_map[t]  = other.no(t).dup
        @in_map[t]  = other.in(t).dup
        @out_map[t] = other.out(t).dup
      }
      self
    end

    def dump(io=$stdout)
      @active_tags.each { |t|
        io.printf("  -- %5s: no=%s,in=%s,out=%s\n",t,@no_map[t].to_s, @in_map[t].to_s, @out_map[t].to_s)
      }
    end
  end

  #
  # Managing persistence information is a little bit challening.
  #
  # The persistence information of scope provides the following queries
  #  1) +locally_persistent(tag)+ tells whether the cache tag is
  #     persistent relative to the scope
  #  2) +preceeding_accesses(tag)+ provides a cache tag set that might
  #     be accessed before the first access to a tag
  #  3) +subscope_persistent(tag, initial)+ tells wether a cache tag
  #     is persistent in a subscope, given the initial state at the entry
  #     of the subscope
  #
  # The queries correspond to the following decision problems:
  #
  # 1) for all path between two accesses in the scope, is there a conflict?
  # 2) what cache tags might be accessed before the first access to t?
  # 3) for all path from an access in a different scope to the first access
  #    in the subscope, is there a conflict?
  #
  #
  class PersistenceInfo

    attr_reader :final_state, :active_tags

    def initialize(rg, results, scope_node, set, active_tags, analysis)
      @scope_node, @set = scope_node, set
      @active_tags = active_tags
      @analysis = analysis
      build(rg, results)
    end

    def preceeding_accesses(t)
      final_state.in(t)
    end

    def locally_persistent?(t)
      @persistent[t]
    end

    def subscope_persistent?(t, results)
      return false unless locally_persistent?(t)
      return results.out(t).join(@final_state.in(t)) != TagSet::TOP
    end

    def dump(io = $stdout)
      io.puts "Persistence Info for #{@scope_node} (set #{@set})"
      io.puts " - Active Tags: #{active_tags}"      
      io.puts " - Local Persistence"
      active_tags.each { |t|
        io.puts "  -- #{t}: #{locally_persistent?(t)}"
      }
      io.puts " - Final State"
      final_state.dump(io)
    end

    private
    #
    # for each node in the region graph
    #  - if it is an access node, check local persistence for the accessed tag
    #  - if it is a subscope node, check subscope persistence
    #  - if it is the exit node, save final state
    #
    def build(region_graph, results)
      @persistent = Hash.new(true)
      region_graph.nodes.each { |n|
        case n
        when RegionGraph::ActionNode
          t = n.action.tag
          @persistent[t] &&= results[n].out(t) != TagSet::TOP
        when RegionGraph::SubScopeNode
          subresults = @analysis.analyze(n.scope_node, @set)
          subresults.active_tags.each { |t|
            @persistent[t] &&= subresults.subscope_persistent?(t, results[n])
          }
        when RegionGraph::ExitNode
          @final_state = results[n]
        end
      }
    end

  end

  def initialize(persistence_analysis, check_conflict)
    @persistence_analysis = persistence_analysis
    @check_conflict = check_conflict
    @results = {}
  end

  def conflicts?(tag_set)
    @check_conflict.call(tag_set)
  end

  # run data flow analysis, and compute persistence
  def analyze(scope_node, set)
    return @results[[scope_node,set]] if @results[[scope_node,set]]
    # HACK (indirect calls not yet supported)
    if(scope_node.kind_of?(ScopeGraph::CallNode))
      assert("persistence analysis does not support indirect calls yet") { scope_node.successors.length == 1 }
      return @results[[scope_node,set]] = analyze(scope_node.successors.first, set)
    end

    active_tags = @persistence_analysis.get_all_tags(scope_node, set).keys

    # final and initial state
    initial_state = CacheSetMap.new(active_tags, @check_conflict)
    final_state = nil

    # goal: compute @inval of exit node
    active_tags = @persistence_analysis.get_all_tags(scope_node, set).keys
    rg = @persistence_analysis.get_region_graph(scope_node)
    @inval = {}
    @inval[rg.entry_node] = initial_state

    ticks = 0
    # we could simple enqueue nodes that changed, but this leads to an intolerable
    # running time for some benchmarks (nsichneu); therefore, we iteratively process
    # nodes in topological order (acyclic graphs ftw :))
    begin
      # info("Starting next iteration of persistence analysis for #{scope_node} (set #{set})")
      restart = false
      topological_sort(rg.entry_node).each { |node|
        ticks += 1
        state = out_state = @inval[node]
        case node
        when RegionGraph::ActionNode
          if @persistence_analysis.cache_properties.set_of(node.action.tag) == set
            out_state = state.dup.access(node.action.tag)
          end
        when RegionGraph::SubScopeNode
          sub_final_state = analyze(node.scope_node, set).final_state
          out_state = state.dup.call(sub_final_state)
        when RegionGraph::RecNode
          new_state, changed = @inval[rg.entry_node].join_with(out_state)
          restart = true if changed
        end
        node.successors.each { |succ|
          if @inval[succ]
            new_state, changed = @inval[succ].join_with(out_state)
            restart = true if changed
          else
            @inval[succ] = out_state.dup
            restart = true
          end
        }
      }
      # reiterate if something changed
    end while restart

    persistence_info = PersistenceInfo.new(rg, @inval, scope_node, set, active_tags, self)
    # info("Results of presistence analysis of #{scope_node}")
    # persistence_info.dump($stderr)
    info("Finished persistence analysis for #{scope_node} (set #{set}): ticks=#{ticks}")

    @results[[scope_node,set]] = persistence_info
  end
end

class PersistenceAnalysis
 
  attr_reader :options

  def initialize(cache_analysis, options)
    @analysis, @options = cache_analysis, options
  end

  def get_check_conflict
    Proc.new { |tagset|
      ! @analysis.cache_properties.conflict_free?(tagset)
    }
  end

  # delegator to CacheRegionAnalysis#get_all_tags
  def get_all_tags(node, set)
    @analysis.get_all_tags(node, set)
  end

  # delegator to CacheRegionAnalysis#cache_properties
  def cache_properties
    @analysis.cache_properties
  end

  # delagator to CacheRegionAnalysis#region_graph
  def get_region_graph(node)
    @analysis.get_region_graph(node)
  end

  #
  # The LRU variant checks for each tag whether it is
  # persistent (~ conflict-free)
  #
  def compute_conflict_free_scopes(scopegraph, set)
    @pdfa = PersistenceDataFlowAnalysis.new(self, get_check_conflict)
    @persistent = {}
    @conflict_free = {}
    scopegraph.bottom_up.each { |node|
      get_all_tags(node, set).each { |tag, load_instructions|        
        if persistent?(node, tag)
          # if the tag is persistent in the node, just mark it (unless it is the root)
          debug(options, :cache) { "Persistent in Scope: #{tag} in #{node}" }
          if node == scopegraph.root
            @analysis.add_scope_for_tag(node, tag)
          end
        else
          # analyze conflict scope
          self.analyze_conflict_scope(node, tag)
        end
      }
    }
  end

  #
  # decide whether a tag is potentiall accessed in a scope
  #
  def accessed?(node, tag)
    all_tags = @analysis.get_all_tags(node,@analysis.cache_properties.set_of(tag)).keys        
    all_tags.include?(tag)
  end
    
  #
  # decide whether a tag is persistent in a scope
  #
  def persistent?(node, tag)
    return @persistent[[node,tag]] unless @persistent[[node,tag]].nil?
    @persistent[[node,tag]] = check_persistence(node, tag)
  end

  def check_persistence(node, tag)
    node.successors.each { |subnode|
      return false unless persistent?(subnode, tag)
    }
    analysis_results = @pdfa.analyze(node, @analysis.cache_properties.set_of(tag))
    analysis_results.locally_persistent?(tag)
  end
  #
  # Find persistent sub scopes in a scope with conflicts
  #
  def analyze_conflict_scope(node, tag)

    debug(options, :cache) { "Tag #{tag} is not persistent in #{node}" }
    # for call nodes, subscopes have been taken care of
    unless node.kind_of?(ScopeGraph::CallNode)
      rg = get_region_graph(node)
      rg.action_nodes.each { |action_node|
        load_instruction = action_node.action
        accessed_tag = load_instruction.tag
        if accessed_tag == tag
          @analysis.add_scope_for_tag(ScopeGraph::BlockNode.new(action_node.block, node.context), tag)
        end
      }
    end
    add_persistent_subscopes(node, tag)
  end

  # account for persistent tags in subscope of conflicting scope
  def add_persistent_subscopes(node, tag)
    # add scope constraints for all conflict-free successors
    node.successors.each { |snode|
      if accessed?(snode, tag) && persistent?(snode, tag)
        @analysis.add_scope_for_tag(snode, tag)
      end
    }
  end
end
