#
# platin tool set
#
# eval-collect: collect evaluation results
#
require 'find'
require 'yaml'
require 'json'
require 'core/pml'
require 'set'
include PML

###################################################################################################
# Customize Below
###################################################################################################

# title
title = "PLATIN evaluation: trace flow-facts + platin-wca + platin-transform + aiT"

# values that should be reported relative to another one
baselines = { 'wca-uptrace-all' => 'wca-trace-all',
              'wca-uptrace-all-size' => 'wca-trace-all-size',
              'wca-uptrace-all-time' => 'wca-trace-all-time',
              'wca-sweet-all'   => 'wca-trace-all',
              'wca-uptrace-local' => 'wca-trace-local',
              'wca-uptrace-local-size' => 'wca-trace-local-size',
              'wca-uptrace-local-time' => 'wca-trace-local-time',
              'wca-sweet-local'   => 'wca-trace-local',
              'wca-uptrace-minimal' => 'wca-trace-minimal',
              'wca-sweet-minimal'   => 'wca-trace-minimal' }
fractions = { 'metric-mcblocks-unmapped' => 'metric-mcblocks' }
filter_dimensions = [ 'arch', 'opt' ]

# correction term for baseline measurement
overhead = 0 # -1

# columns (in order), and labels for columns
column_labels = [ [ 'mod'      , 'Name' ],
                  [ 'arch'     , 'Arch' ],
                  [ 'opt'      , 'Opt'  ],
                  [ 'metric-functions' , 'F' ],
                  [ 'metric-bcblocks'  , 'V_ir' ],
                  [ 'metric-mcblocks-unmapped'  , 'V_ir/u' ],
                  [ 'metric-mcblocks'  , 'V_mc' ],
                  [ 'metric-mcblocks-unmapped', 'V_mc/u' ],
                  [ 'metric-rgblocks'  , 'V_rel' ],
                  [ 'metric-rgprogress', 'V_p' ],
                  [ 'metric-rgsrc'     , 'V_a' ],
                  [ 'metric-rgdst'     , 'V_b' ],
                  [ 'metric-rtblocks'  , 'V_asm' ],
                  [ 'trace'             , 'sim' ] ]
[ ["all",""], ["local","/local"],["minimal","/min"] ].each { |sel,sel_suffix|
  [ ['',''], ['-time','t:'], ['-size','s:' ] ].each { |fact,fact_prefix|
    [ ["wca-trace","wca/tr"],["wca-uptrace","wca/tf"],["wca-sweet","wca/sweet"],["aiT-trace","aiT/tr"] ].each { |wca,wca_label|
      entry,entry_label = "#{wca}-#{sel}#{fact}", "#{fact_prefix}#{wca_label}#{sel_suffix}"
      column_labels.push([entry,entry_label])
    }
  }
}

# legend
legend = <<EOF
name ... Name of the benchmark
_di  ... Disable if-conversion
F     ... Number of functions in the analyzed part of the program
V_ir  ... IR-level basic blocks
V_mc  ... MC-level basic blocks (without compiler-rt)
V_rg  ... Number of Relation Graph Nodes
V_asm ... MC-level basic blocks (compiler-rt)

trace     ... Cycles to execute benchmark using *pasim* (simple timing model)
wca/      ... WCET-estimate calculated using platin-wca
aiT/      ... WCET-estimate calculated using a3patmos
*/tr      ... Flow facts generated from trace (valid for benchmark run only)
*/tf      ... 'tr' flow facts transformed to bitcode level using relation graphs
*/*/local ... Only use intraprocedural flow facts
*/*/min   ... Only use flow facts corresponding to (a) loop bounds (b) infeasible blocks
              and (c) indirect call targets
EOF

###################################################################################################
# returns [relative_value, relative_value_repr] or nil
def filter(e) ; true ; end
def relative_change(value,base)
  return value if ! value || ! base || value <= 0
  rel = ((value.to_f/base) - 1) * 100
  return '=' if rel == 0
  return sprintf("%s%.2f\%",rel>0?"+":"-",rel.abs)
end
def preferred_order(keys,known_keys)
  todo = Set.new(keys)
  ordered_keys = []
  known_keys.each { |k|
    ordered_keys.push(k) if(todo.include?(k))
    todo.delete(k)
  }
  todo.each { |k| ordered_keys.push(k) }
  ordered_keys
end

module Enumerable
  def sum
    self.inject(0,:+)
  end
end
def relation_nodes(pml, f)
  pml.relation_graphs.by_name(f.name, :src).nodes.select { |n| yield n }
end
if __FILE__ == $0
  if ARGV.length < 1
    puts "Usage: #{$0} resultdir.."
    exit 1
  end
  # crawl all files in the result dir
  data = []
  ARGV.each do |rdir|
    Find.find(rdir) do |path|
      if FileTest.directory?(path)
        if File.basename(path)[0] == '.' || File.basename(path) == "bin"
          Find.prune       # Don't look any further into this directory.
        end
      elsif path =~ %r{/([^/]+)/([^/]+)/out/([^/]+).pml}
        arch,opt,mod = $1,$2,$3
        data.push({'path'=>path,'arch'=>arch,'opt'=>opt,'mod'=>mod})
      end
    end
  end
  keys = %w{mod arch opt}
  data.sort_by! { |d| keys.map { |k| d[k] } }
  allkeys=Set.new
  keys.each { |k| allkeys.add(k) }
  data.each_with_index do |entry,ix|
    fn = entry.delete('path')
    $stderr.puts("Reading #{fn}")
    values = {}
    File.open(fn) { |fh|
      pml = PMLDoc.new(YAML::load_stream(fh))

      entry_bc, entry_mc = "main", pml.machine_functions.by_label("main").name
      fs_bc = pml.bitcode_functions.reachable_from("main").first
      fs_mc = pml.machine_functions.reachable_from(entry_mc).first
      fs_mc_mapped = fs_mc.select { |f| fs_bc.any? { |fbc| fbc.name == f.label } }
      fs_mc_unmapped = fs_mc.reject { |f| fs_mc_mapped.include?(f) }

      v_bc = fs_bc.map { |f| f.blocks.length }.sum
      v_mc = fs_mc_mapped.map { |f| f.blocks.length }.sum
      v_mc_unmapped = fs_mc_unmapped.map { |f| f.blocks.length }.sum
      v_rg = fs_bc.map { |f|  relation_nodes(pml,f) { |n| true }.length }.sum
      v_progress = fs_bc.map { |f|  relation_nodes(pml,f) { |n| n.type == :progress }.length }.sum
      v_src = fs_bc.map { |f| relation_nodes(pml,f) { |n| n.type == :src }.length }.sum
      v_dst = fs_bc.map { |f| relation_nodes(pml,f) { |n| n.type == :dst }.length }.sum
      v_bc_u = fs_bc.map { |f| relation_nodes(pml,f) { |n| n.type == :src }.map { |n| n.get_block(:src) }.uniq.length }.sum
      v_mc_u = fs_bc.map { |f| relation_nodes(pml,f) { |n| n.type == :dst }.map { |n| n.get_block(:dst) }.uniq.length }.sum
      entry["metric-functions"] = fs_bc.length
      entry["metric-bcblocks"] = v_bc
      entry['metric-mcblocks-unmapped'] = v_bc_u
      entry['metric-mcblocks'] = v_mc
      entry['metric-mcblocks-unmapped'] = v_mc_u
      entry['metric-rtblocks'] = v_mc_unmapped
      entry['metric-rgblocks'] = v_rg
      entry['metric-rgprogress'] = v_progress
      entry['metric-rgsrc'] = v_src
      entry['metric-rgdst'] = v_dst
      visited = {}
      pml.timing.each { |te|
        if visited[te.origin]
          $stderr.puts("Skipping duplicate entry #{te}")
          next
        else
          visited[te.origin] = true
        end
        value = te.cycles
        value = nil if te.cycles <= 0
        entry[te.origin] = value
        entry[te.origin+"-size"] = te['num_constraints'] if te['num_constraints']
        entry[te.origin+"-time"] = te['solvertime'] if te['solvertime']
      }
      entry.keys.each { |k|
        if(! allkeys.include?(k))
          keys.push(k)
          allkeys.add(k)
        end
      }
    }
  end
  data.select! do |entry|
    allkeys.each do |k|
      entry[k] = 'x' unless entry[k]
    end
    filter(entry)
  end
  keys = preferred_order(keys,column_labels.map { |k,v| k })
  labelmap = column_labels.inject({}) { |ht,d| ht[d.first] = d[1]; ht }
  labels = keys.map { |k| labelmap[k] || k }
  puts({"keys"=>keys,"labels"=>labels,"data"=>data,"legend"=>legend,'baselines'=>baselines,'fractions'=>fractions,'dimensions'=>filter_dimensions}.to_json)
end
