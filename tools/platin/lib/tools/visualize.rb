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
  def visualize_vcfg(function, arch)
    g = GraphViz.new( :G, :type => :digraph )
    g.node[:shape] = "rectangle"
    vcfg = VCFG.new(function, arch)
    name = function.name.to_s
    name << "/#{function.mapsto}" if function.mapsto
    g[:label] = "CFG for " + name
    nodes = {}
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
      elsif node.kind_of?(LoopStateNode)
        label += "LOOP #{node.action} #{node.loop.name}"
      end
      nodes[nid] = g.add_nodes(nid.to_s, :label => label)
    end
    vcfg.nodes.each do |node|
      node.successors.each do |s|
        g.add_edges(nodes[node.nid],nodes[s.nid])
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
      if options.show_calls
        block.instructions.each do |ins|
          unless ins.callees.empty?
            label << "\n " << ins.callees.map { |c| "#{c}()" }.join(",")
          end
        end
      end
      #    block.instructions.each do |ins|
      #      label << "\n#{ins.opcode} #{ins.size}"
      #    end
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
  VALID_FORMATS = GraphViz::Constants::FORMATS

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
        graph = fgv.visualize_vcfg(mf, pml.arch)
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
    opts.on("-O","--outdir DIR","Output directory for image files") { |d| opts.options.outdir = d }
    opts.on("--graphviz-format FORMAT", "GraphViz output format (=png,svg,...)") { |format|
      options.graphviz_format = format
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

