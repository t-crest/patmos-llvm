#!/usr/bin/env ruby
#
# PLATIN tool set
#
# Simple visualizer (should be expanded to do proper report generation)
#
require 'set'
require 'platin'
require 'analysis/scopegraph'
include PML

begin
  require 'rubygems'
  require 'graphviz'
rescue Exception => details
  warn "Failed to load library graphviz"
  info "  ==> gem1.9.1 install ruby-graphviz"
  die "Failed to load required ruby libraries"
end

class Visualizer
  attr_reader :options
  def generate(g,outfile)
    debug(options, :visualize) { "Generating #{outfile}" }
    g.output( options.graphviz_format.to_sym => "#{outfile}" )
    info("#{outfile} ok") if options.verbose
  end
protected
  def digraph(label)
    g = GraphViz.new(:G, :type => :digraph)
    g.node[:shape] = "rectangle"
    g[:label] = label
    g
  end
end

class CallGraphVisualizer < Visualizer
  def initialize(pml, options) ; @pml, @options = pml, options ; end
  def visualize_callgraph(function)
    g  = digraph("Callgraph for #{function}")
    refinement = ControlFlowRefinement.new(function, 'machinecode')
    cg = ScopeGraph.new(function, refinement, @pml, @options).callgraph
    nodes, nids = {}, {}
    cg.nodes.each_with_index { |n,i| nids[n] = i }
    cg.nodes.each { |node|
      nid = nids[node]
      label = node.to_s
      nodes[node] = g.add_nodes(nid.to_s, :label => label)
    }
    cg.nodes.each { |n|
      n.successors.each { |s|
        g.add_edges(nodes[n],nodes[s])
      }
    }
    g
  end
end

class ScopeGraphVisualizer < Visualizer
  def initialize(pml, options) ; @pml, @options = pml, options ; end
  def visualize_scopegraph(function)
    g  = digraph("Scopegraph for #{function}")
    refinement = ControlFlowRefinement.new(function, 'machinecode')
    sg = ScopeGraph.new(function, refinement, @pml, @options)
    nodes, nids = {}, {}
    sg.nodes.each_with_index { |n,i| nids[n] = i }
    sg.nodes.each { |node|
      nid = nids[node]
      label = node.to_s
      nodes[node] = g.add_nodes(nid.to_s, :label => label)
    }
    sg.nodes.each { |n|
      n.successors.each { |s|
        g.add_edges(nodes[n],nodes[s])
      }
    }
    g
  end
end

class FlowGraphVisualizer < Visualizer
  def initialize(pml, options) ; @pml, @options = pml, options ; end
  def extract_timing(function, timing)
    Hash[ 
      timing.select{ |t| t.profile }.map { |t|
	profile = {}
	t.profile.select { |e|
	  e.reference.function == function
	}.each { |e|
	  next unless e
	  edge = e.reference.programpoint
	  # We only keep edge profiles here
	  next unless edge.kind_of?(Edge)
	  profile[edge.source]||=[]
	  profile[edge.source].push(e)
	}
	[t.origin, profile]
      }.select{ |k,v| not v.empty? }
    ]
  end
  def get_vblocks(node, adjacentcy)
    [*node].map { |n|
      n.block || get_vblocks(n.send(adjacentcy), adjacentcy)
    }.flatten
  end
  def find_vnode_timing(profile, node)
    if node.block
      profile[node.block]||[]
    else
      find_vedge_timing(profile, node.predecessors, node.successors)
    end
  end
  def find_vedge_timing(profile, node, succ)
    start = Set.new( get_vblocks(node, :predecessors) )
    targets = Set.new( get_vblocks(succ, :successors) )
    if start == targets and not [*succ].any? { |s| s.kind_of?(CfgNode) and s.block_start? }
      [*node].map { |n| find_vnode_timing(profile, n) }.flatten
    elsif succ.kind_of?(ExitNode)
      start.map{ |b| profile[b]||[] }.flatten.select { |t|
        t.reference.programpoint.exitedge?
      }
    else
      start.map{ |b| profile[b]||[] }.flatten.select { |t|
	targets.include?( t.reference.programpoint.target )
      }
    end
  end
  def visualize_vcfg(function, arch, timing=nil)
    g = GraphViz.new( :G, :type => :digraph )
    g.node[:shape] = "rectangle"
    vcfg = VCFG.new(function, arch)
    name = function.name.to_s
    name << "/#{function.mapsto}" if function.mapsto
    g[:label] = "CFG for " + name
    nodes = {}
    block_timing = extract_timing(function, timing)
    sf_headers = function.subfunctions.map { |sf| sf.entry }
    vcfg.nodes.each do |node|
      nid = node.nid
      label = "" # "[#{nid}] "
      if node.kind_of?(EntryNode)
        label += "START"
      elsif node.kind_of?(ExitNode)
        label += "END"
      elsif node.kind_of?(CallNode)
        label += "CALL #{node.callsite.callees.map { |c| "#{c}()" }.join(",")}"
      elsif node.kind_of?(BlockSliceNode)
        block = node.block
        addr = block.instructions[node.first_index].address
        label += sprintf("0x%x: ",addr) if addr
        label += "#{block.name}"
        label << "(#{block.mapsto})" if block.mapsto
        label << " [#{node.first_index}..#{node.last_index}]"
	if @options.show_instructions
	  node.instructions.each do |ins|
	    label << "\\l#{ins.opcode} #{ins.size}"
	  end
	  label << "\\l"
	end
      elsif node.kind_of?(LoopStateNode)
        label += "LOOP #{node.action} #{node.loop.name}"
      end
      options = { :label => label }
      if node.block_start? and sf_headers.include?(node.block)
	# Mark subfunction headers
	options["fillcolor"] = "#ffffcc"
	options["style"] = "filled"
      elsif not node.block or node.kind_of?(CallNode)
        options["style"] = "rounded"
      end
      if block_timing.any?{ |o,profile| find_vnode_timing(profile, node).any? { |e| e.wcetfreq > 0 } }
        # TODO visualize criticality < 1
	options["color"] = "#ff0000"
	options["penwidth"] = 2
      end
      nodes[nid] = g.add_nodes(nid.to_s, options)
    end
    vcfg.nodes.each do |node|
      node.successors.each do |s|
        options = {}
	# Find WCET results for edge
	# TODO It can be the case that there are multiple edges from different slices
	#      of the same block to the same target. This is actually just a single edge
	#      in PML, but we annotate all edges here, making it look as we would count
	#      them twice. No easy way to fix this tough.. If we choose to annotate only
	#      one of those edges here, it should be edge with the longest path through
	#      the block at least.
	t = block_timing.map{ |origin,profile| 
	  [origin, find_vedge_timing(profile, node, s).select{ |e| e.wcetfreq > 0 } ]
	}.select{ |o,p| not p.empty? }
	if not t.empty?
	  # TODO visualize criticality < 1
	  options["color"] = "#ff0000"
	  options["penwidth"] = 2
	  # Annotate frequency and cycles only to 'real' edges between blocks
	  # These are edges from a block node to either a different block, a self loop, 
	  # or edges to virtual nodes (assuming the VCFG does not insert virtual nodes
	  # within a block)
	  if node.block and ( node.block != s.block or s.block_start? )
	    options["label"] = ""
	    t.each do |origin, profile|
	      # TODO We need a way to merge results from different contexts properly.
	      #      aiT returns multiple loop context results, frequencies must
	      #      be merged properly for sub-contexts.
	      #      Merging timing results should go into core library functions.
	      #      We might need to merge differently depending on origin!!
	      #      In that case, ask the ext plugins (aiT,..) to do the work.
	      freq, cycles, wcet, crit = profile.inject([0,0,0,0]) { |v,e|
		freq, cycles, wcet, crit = v
		[freq + e.wcetfreq, 
		 [cycles, e.cycles].max, 
		 wcet + e.wcet_contribution,
		 [crit, e.criticality || 1].max
		]
	      }
	      # Avoid overlapping of the first character and the edge by starting 
	      # the label with a space
	      options["label"] += "\\l" if options["label"] != ""
	      options["label"] += " -- #{origin} --" if block_timing.length > 1
	      options["label"] += "\\l" if options["label"] != ""
	      options["label"] += " f = #{freq}"
	      options["label"] += "\\l max = #{cycles} cycles"
	      options["label"] += "\\l sum = #{wcet} cycles"
	      options["label"] += "\\l crit = #{crit}" if crit < 1
	    end
	    options["label"] += "\\l"
	  end
	end
        g.add_edges(nodes[node.nid],nodes[s.nid],options)
      end
    end
    g
  end
  def visualize_cfg(function)
    g = GraphViz.new( :G, :type => :digraph )
    g.node[:shape] = "rectangle"
    name = function.name.to_s
    name << "/#{function.mapsto}" if function.mapsto
    g[:label] = "CFG for " + name
    nodes = {}
    function.blocks.each do |block|
      bid = block.name
      label = "#{block.name}"
      label << " (#{block.mapsto})" if block.mapsto
#      label << " L#{block.loops.map {|b| b.loopheader.name}.join(",")}" unless block.loops.empty?
      label << " |#{block.instructions.length}|"
      if @options.show_calls
        block.instructions.each do |ins|
          unless ins.callees.empty?
            label << "\\l call " << ins.callees.map { |c| "#{c}()" }.join(",")
          end
        end
      end
      if @options.show_instructions
	block.instructions.each do |ins|
	  label << "\\l#{ins.opcode} #{ins.size}"
	end
	label << "\\l"
      end
      nodes[bid] = g.add_nodes(bid.to_s, :label => label,
                               :peripheries => block.loops.length + 1)
    end
    function.blocks.each do |block|
      block.successors.each do |s|
        g.add_edges(nodes[block.name],nodes[s.name])
      end
    end
    g
  end
end
class RelationGraphVisualizer < Visualizer
  def initialize(options) ; @options = options ; end
  def visualize(rg)
    nodes = {}
    g = GraphViz.new( :G, :type => :digraph )
    g.node[:shape] = "rectangle" 

    # XXX: update me
    rg = rg.data if rg.kind_of?(RelationGraph)      

    name = "#{rg['src'].inspect}/#{rg['dst'].inspect}"
    rg['nodes'].each do |node|
      bid = node['name']
      label = "#{bid} #{node['type']}"
      label << " #{node['src-block']}" if node['src-block']
      label << " #{node['dst-block']}" if node['dst-block']
      nodes[bid] = g.add_nodes(bid.to_s, :label => label)
    end
    rg['nodes'].each do |node|
      bid = node['name']
      (node['src-successors']||[]).each do |sid|
        g.add_edges(nodes[bid],nodes[sid])
      end
      (node['dst-successors']||[]).each do |sid|
        g.add_edges(nodes[bid],nodes[sid], :style => 'dotted')
      end
    end
    g
  end
end

# HTML Index Pages for convenient debugging
# XXX: quick hack
class HtmlIndexPages
  def initialize
    @targets, @types = {}, Set.new
  end
  def add(target,type,image)
    (@targets[target] ||= {})[type] = image
    @types.add(type)
  end
  def generate(outdir)
    @targets.each do |target,images|
      images.each do |type,image|
        File.open(File.join(outdir,link(target,type)),"w") do |fh|
          fh.puts("<html><head><title>#{target} #{type}</title></head><body>")
          type_index(target,type,fh)
          target_index(target,type,fh)
          image_display(File.basename(image), fh)
          fh.puts("</body></html>")
        end
      end
    end
  end
  private
  def link(target,type)
    "#{target}.#{type}.html"
  end
  def type_index(selected_target, selected_type, io)
    io.puts("<div>")
    @targets[selected_target].each do |type,image|
      style = if type==selected_type then "background-color: lightblue;" else "" end
      io.puts("<span style=\"#{style}\"><a href=\"#{link(selected_target,type)}\">#{type}</a></span>")
    end
    io.puts("</div>")
  end
  def target_index(selected_target, selected_type, io)
    io.puts("<div>")
    @targets.each do |target,images|
      type, image = images.find { |type,image| type == selected_type } || images.to_a.first
      style = if target==selected_target then "background-color: lightblue;" else "" end
      io.puts("<span style=\"#{style}\"><a href=\"#{link(target,type)}\">#{target}</a></span>")
    end
    io.puts("</div>")
  end
  def image_display(ref, io)
    io.puts("<image src=\"#{ref}\"/>")
  end
end
class VisualizeTool
  # TODO use GraphViz::Constants::FORMATS? Packaged ruby-graphviz 1.0.8 defines
  #      Constants at top-level instead of inside GraphViz, giving a ruby warning (?)
  VALID_FORMATS = Constants::FORMATS

  def VisualizeTool.default_targets(pml)
    entry = pml.machine_functions.by_label("main")
    pml.machine_functions.reachable_from(entry.name).first.reject { |f|
      f.label =~ /printf/
    }.map { |f|
      f.label
    }
  end
  def VisualizeTool.run(pml, options)
    targets = options.functions || VisualizeTool.default_targets(pml)
    outdir = options.outdir || "."
    options.graphviz_format ||= "png"
    suffix = "." + options.graphviz_format
    html = HtmlIndexPages.new if options.html
    targets.each do |target|

      # Visualize call graph
      cgv = CallGraphVisualizer.new(pml, options)
      begin
        mf = pml.machine_functions.by_label(target)
        graph = cgv.visualize_callgraph(mf)
        file = File.join(outdir, target + ".cg" + suffix)
        cgv.generate(graph , file)
        html.add(target,"cg",file) if options.html
      rescue Exception => detail
        puts "Failed to visualize callgraph for #{target}: #{detail}"
        puts detail.backtrace
        raise detail if options.raise_on_error
      end
      # Visualize Scope Graph
      sgv = ScopeGraphVisualizer.new(pml,options)
      begin
        mf = pml.machine_functions.by_label(target)
        graph = sgv.visualize_scopegraph(mf)
        file = File.join(outdir, target + ".sg" + suffix)
        sgv.generate(graph , file)
        html.add(target,"sg",file) if options.html
      rescue Exception => detail
        puts "Failed to visualize scopegraph for #{target}: #{detail}"
        puts detail.backtrace
        raise detail if options.raise_on_error
      end
      # Visualize CFG (bitcode)
      fgv = FlowGraphVisualizer.new(pml, options)
      begin
        bf = pml.bitcode_functions.by_name(target)
        file = File.join(outdir, target + ".bc" + suffix)
        fgv.generate(fgv.visualize_cfg(bf),file)
        html.add(target,"bc",file) if options.html
      rescue Exception => detail
        puts "Failed to visualize bitcode function #{target}: #{detail}"
        raise detail
      end
      # Visualize VCFG (machine code)
      begin
        mf = pml.machine_functions.by_label(target)
	t = pml.timing.select { |t| 
	  t.level == mf.level && options.show_timings &&
	  (options.show_timings.include?(t.origin) || options.show_timings.include?("all"))
	}
        graph = fgv.visualize_vcfg(mf, pml.arch, t)
        file = File.join(outdir, target + ".mc" + suffix)
        fgv.generate(graph , file)
        html.add(target,"mc",file) if options.html
      rescue Exception => detail
        puts "Failed to visualize machinecode function #{target}: #{detail}"
        puts detail.backtrace
        raise detail if options.raise_on_error
      end
      # Visualize relation graph
      begin
        rgv = RelationGraphVisualizer.new(options)
        rg = pml.data['relation-graphs'].find { |f| f['src']['function'] ==target or f['dst']['function'] == target }
        raise Exception.new("Relation Graph not found") unless rg
        file = File.join(outdir, target + ".rg" + suffix)
        rgv.generate(rgv.visualize(rg),file)
        html.add(target,"rg",file) if options.html
      rescue Exception => detail
        puts "Failed to visualize relation graph of #{target}: #{detail}"
        raise detail if options.raise_on_error
      end
    end
    html.generate(outdir) if options.html
    statistics("VISUALIZE","Generated bc+mc+rg graphs" => targets.length) if options.stats
  end

  def VisualizeTool.add_options(opts)
    opts.on("--[no-]html","Generate HTML index pages") { |b| opts.options.html = b }
    opts.on("-f","--function FUNCTION,...","Name of the function(s) to visualize") { |f| opts.options.functions = f.split(/\s*,\s*/) }
    opts.on("--show-calls", "Visualize call sites") { opts.options.show_calls = true }
    opts.on("--show-instr", "Show instructions in basic block nodes") { opts.options.show_instructions = true }
    opts.on("--show-timings [ORIGIN]", Array, "Show timing results in flow graphs (=all; can be a list of origins))") { |o| 
      opts.options.show_timings = o ? o : ["all"]
    }
    opts.on("-O","--outdir DIR","Output directory for image files") { |d| opts.options.outdir = d }
    opts.on("--graphviz-format FORMAT", "GraphViz output format (=png,svg,...)") { |format|
      opts.options.graphviz_format = format
    }
    opts.add_check { |options|
      options.graphviz_format ||= "png"
      unless VisualizeTool::VALID_FORMATS.include?(options.graphviz_format)
        info("Valid GraphViz formats: #{VisualizeTool.VALID_FORMATS.join(", ")}")
        die("Bad GraphViz format: #{options.graphviz_format}")
      end
    }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Visualize bitcode and machine code CFGS, and the control-flow relation
graph of the specified set of functions
EOF
  options, args = PML::optparse([],"", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.callstring_length
    VisualizeTool.add_options(opts)
  end
  VisualizeTool.run(PMLDoc.from_files(options.input), options)
end

