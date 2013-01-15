#!/usr/bin/env ruby
#
# This is debugging only; ok to remove from repo at some point
#
begin
  require 'rubygems'
  require 'graphviz'
rescue Exception => details
  warn "Failed to load library graphviz"
  info "  ==> gem1.9.1 install ruby-graphviz"
  die "Failed to load required ruby libraries"
end
require 'utils.rb'
include PML

class Visualizer
  def generate(object,outfile)
    g = visualize(object)
    puts outfile
    g.output( :png => "#{outfile}" )
    $stderr.puts "#{outfile} ok"
  end
end
class FlowGraphVisualizer < Visualizer
  def visualize(function)
    g = GraphViz.new( :G, :type => :digraph )
    g.node[:shape] = "rectangle"
    name = function['name'].to_s
    name << "/#{function['mapsto']}" if function['mapsto']
    nodes = {}
    function.blocks.each do |block|
      bid = block.name
      label = "#{block.name}"
      label << " (#{block['mapsto']})" if block['mapsto']
      label << " L#{block['loops'].join(",")}" unless (block['loops']||[]).empty?
      label << " |#{block.instructions.length}|"
      #    block['instructions'].each do |ins|
      #      label << "\n#{ins['opcode']} #{ins['size']}"
      #    end
      nodes[bid] = g.add_nodes(bid.to_s, :label => label)
    end
    function.blocks.each do |block|
      bid = block.name
      (block['successors']||[]).each do |s|
        g.add_edges(nodes[bid],nodes[s])
      end
    end
    g
  end
end
class RelationGraphVisualizer < Visualizer
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
  def VisualizeTool.run(pml, options)
    target = options.function || "main"
    outdir = options.outdir || "."
    # Visualize the bitcode, machine code and relation graphs
    fgv = FlowGraphVisualizer.new
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
      rgv = RelationGraphVisualizer.new
      rg = pml.data['relation-graphs'].find { |f| f['src']['function'] ==target or f['dst']['function'] == target }
      raise Exception.new("Relation Graph not found") unless rg
      rgv.generate(rg,File.join(outdir, target + ".rg" + ".png"))
    rescue Exception => detail
      puts "Failed to visualize relation graph of #{target}: #{detail}"
    end
  end
  def VisualizeTool.add_options(opts,options)
    opts.on("-f","--function FUNCTION","Name of the function to visualize") { |f| options.function = f }
    opts.on("-O","--outdir DIR","Output directory for image files") { |d| options.outdir = d }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Visualize bitcode and machine code CFGS, and the control-flow relation
graph of the specified set of functions
EOF
  options, args = PML::optparse(1,"FILE.pml", SYNOPSIS, :type => :none) do |opts,options|
    VisualizeTool.add_options(opts,options)
  end
  VisualizeTool.run(PMLDoc.from_file(args.first), options)
end

