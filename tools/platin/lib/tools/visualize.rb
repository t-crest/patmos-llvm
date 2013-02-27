#!/usr/bin/env ruby
#
# PLATIN tool set
#
# Simple visualizer (should be expanded to do proper report generation)
#
require 'platin.rb'
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
  def generate(object,outfile)
    g = visualize(object)
    $dbgs.puts outfile if options.debug
    g.output( :png => "#{outfile}" )
    $stderr.puts "#{outfile} ok" if options.verbose
  end
end
class FlowGraphVisualizer < Visualizer
  def initialize(options) ; @options = options ; end
  def visualize(function)
    g = GraphViz.new( :G, :type => :digraph )
    g.node[:shape] = "rectangle"
    name = function['name'].to_s
    name << "/#{function['mapsto']}" if function['mapsto']
    g[:label] = "CFG for " + name
    nodes = {}
    function.blocks.each do |block|
      bid = block.name
      label = "#{block.name}"
      label << " (#{block['mapsto']})" if block['mapsto']
      label << " L#{block.loops.map {|b| b.name}.join(",")}" unless block.loops.empty?
      label << " |#{block.instructions.length}|"
      #    block['instructions'].each do |ins|
      #      label << "\n#{ins['opcode']} #{ins['size']}"
      #    end
      nodes[bid] = g.add_nodes(bid.to_s, :label => label)
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

class VisualizeTool
  def VisualizeTool.default_targets(pml)
    pml.bitcode_functions.reachable_from("main").first.map { |f| 
      f.name
    }.reject { |f|
      f =~ /printf/
    }
  end
  def VisualizeTool.run(pml, options)
    targets = options.functions || VisualizeTool.default_targets(pml)
    outdir = options.outdir || "."
    targets.each do |target|
      # Visualize the bitcode, machine code and relation graphs
      fgv = FlowGraphVisualizer.new(options)
      begin
        bf = pml.bitcode_functions.by_name(target)
        fgv.generate(bf,File.join(outdir, target + ".bc" + ".png")) 
      rescue Exception => detail
        puts "Failed to visualize bitcode function #{target}: #{detail}"
        raise detail
      end
      begin
        mf = pml.machine_functions.by_label(target)
        fgv.generate(mf,File.join(outdir, target + ".mc" + ".png"))
      rescue Exception => detail
        puts "Failed to visualize machinecode function #{target}: #{detail}"
      end
      begin
        rgv = RelationGraphVisualizer.new(options)
        rg = pml.data['relation-graphs'].find { |f| f['src']['function'] ==target or f['dst']['function'] == target }
        raise Exception.new("Relation Graph not found") unless rg
        rgv.generate(rg,File.join(outdir, target + ".rg" + ".png"))
      rescue Exception => detail
        puts "Failed to visualize relation graph of #{target}: #{detail}"
      end
    end
    statistics("number of generated bc,mc,rg graphs" => targets.length) if options.stats
  end

  def VisualizeTool.add_options(opts)
    opts.on("-f","--function FUNCTION,...","Name of the function(s) to visualize") { |f| opts.options.functions = f.split(/\s*,\s*/) }
    opts.on("-O","--outdir DIR","Output directory for image files") { |d| opts.options.outdir = d }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Visualize bitcode and machine code CFGS, and the control-flow relation
graph of the specified set of functions
EOF
  options, args = PML::optparse([:input],"FILE.pml", SYNOPSIS) do |opts|
    VisualizeTool.add_options(opts)
  end
  VisualizeTool.run(PMLDoc.from_file(args.first), options)
end

