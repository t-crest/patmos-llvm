#!/usr/bin/env ruby
#
# This is debugging only; ok to remove from repo if ruby is eliminated
# from the set of source languages

begin
  require 'rubygems'
  require 'graphviz'
rescue => detail
  $stderr.puts("you need to 'gem install graphviz' in order to run this program")
  raise detail
end
require 'ostruct'
require 'optparse'
require 'yaml'

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
    function['blocks'].each do |block|
      bid = block['name']
      label = "#{block['name']}"
      label << " (#{block['mapsto']})" if block['mapsto']
      label << " L#{block['loops'].join(",")}" unless (block['loops']||[]).empty?
      label << " |#{block['instructions'].size}|"
      #    block['instructions'].each do |ins|
      #      label << "\n#{ins['opcode']} #{ins['size']}"
      #    end
      nodes[bid] = g.add_nodes(bid.to_s, :label => label)
    end
    function['blocks'].each do |block|
      bid = block['name']
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

# Standard option parser
options = OpenStruct.new
options.function="main"
options.outdir="."
parser = OptionParser.new do |opts|
  opts.banner = "Usage: #{$0} [OPTIONS] file.pml"
  opts.on("-f","--function FUNCTION","Name of the function to visualize") { |f| options.function = f }
  opts.on("-O","--outdir DIR","Output directory for image files") { |d| options.outdir = d }
  opts.on_tail("-h", "--help", "Show this message") { $stderr.puts opts; exit 0 }
end.parse!
if ARGV.length != 1 then $stderr.puts "Wrong number of arguments. Try --help" ; exit 1 ; end
data = YAML::load(ARGV.first ? File.new(ARGV.first) : $<)

# Visualize the bitcode, machine code and relation graphs
fgv = FlowGraphVisualizer.new
bf = data['bitcode-functions'].find { |f| f['name']==options.function }
fgv.generate(bf,File.join(options.outdir, options.function + ".bc" + ".png")) if bf
mf = data['machine-functions'].find { |f| f['name']==options.function or f['mapsto']==options.function }
fgv.generate(mf,File.join(options.outdir, options.function + ".mc" + ".png")) if mf
rgv = RelationGraphVisualizer.new
rg = data['relation-graphs'].find { |f| f['src']['function'] ==options.function or f['dst']['function'] == options.function }
rgv.generate(rg,File.join(options.outdir, options.function + ".rg" + ".png")) if rg
