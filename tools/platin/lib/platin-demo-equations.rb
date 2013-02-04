#!/usr/bin/env ruby
#
# Small demo printing CFRG and Flow Fact equations
# TODO: this is just for playing around, and will be removed eventually

require 'ostruct'
require 'optparse'
require 'yaml'
require 'set'

# Standard option parser
options = OpenStruct.new
parser = OptionParser.new do |opts|
  opts.banner = "Usage: #{$0} file.pml"
  opts.on_tail("-h", "--help", "Show this message") { $stderr.puts opts; exit 0 }
end.parse!
if ARGV.length > 1 then $stderr.puts "Wrong number of arguments. Try --help" ; exit 1 ; end

data = YAML::load(ARGV.first ? File.new(ARGV.first) : $<)

# small helpers to specify equations
class LinearCombination
  attr_reader :lc
  def initialize(terms=[])
    @lc = Hash.new(0)
    terms.each { |t| add_term(t) }
  end
  def add_term(t)
    if t.kind_of?(Array)
      @lc[t[0]] += t[1]
    else
      @lc[t] += 1
    end
    self
  end
  def add_lc(lc)
    lc.lc.each { |k,v| @lc[k] += v }
    self
  end
  def negate
    LinearCombination.new(@lc.map {|k,v| [k,-v] })
  end
  def normalize!
    @lc.delete_if { |k,v| v == 0 }
  end
  def partition
    lcs = [LinearCombination.new, LinearCombination.new]
    @lc.each do |k,v|
      lcs[(yield [k,v]) ? 0 : 1].add_term([k,v])
    end
    lcs
  end
  def to_cmp_str(cmpop)
    lhs,rhs = partition { |k,v| v >= 0 }
    "#{lhs.to_s} #{cmpop} #{rhs.negate.to_s}"
  end
  def to_s
    normalize!
    @lc.map { |k,v|
      k.inspect
      if k && (v==1) then k.to_s
      elsif ! k then v.to_s
      else "#{v} #{k}"
      end
    }.join(" + ")
  end
end
require 'set'
class ConstraintSystem
  attr_reader :eqs,:ineqs
  def initialize
    @eqs,@ineqs = Set.new,Set.new
  end
  def dump
    eqs.each { |lc| puts "  " + lc.to_cmp_str("=") }
    ineqs.each { |lc| puts "  " + lc.to_cmp_str("<=") }
  end
end
class NodeLabeller
  attr_accessor :function,:nodes
  def initialize(rg)
    @function = ['src','dst'].inject(Hash.new) { |ht,lev|
      ht[lev] = rg[lev]['function']
      ht
    }
    @nodes = rg['nodes']
  end
  def label_at (nid,ty)
    label(@nodes[nid],ty)
  end
  def label(node,ty)
    label = node["#{ty}-block"]
    label = "exit" if node['type'] == 'exit'
    #    "#{ty}::#{function[ty]}::#{label}"
    "#{ty}::#{label}"
  end
  def scope(ty)
    "#{ty}::#{function[ty]}()"
  end
end

# Save CFGS
functions = { 'src' => {}, 'dst' => {} }
dstfunmap = {}
data['bitcode-functions'].each { |f| functions['src'][f['name']] = f }
data['machine-functions'].each { |f|
  functions['dst'][f['name']] = f
  dstfunmap[f['mapsto']] = f
}

# Collect called functions
called = Set.new
queue = [dstfunmap['main']]
begin
  while f = queue.pop
    next if called.include?(f['name'])
    called.add(f['name'])
    next unless f['blocks']
    f['blocks'].each do |b|
      b['instructions'].each do |i|
        raise Exception.new if (i['callees']||[]).include?("__any__")
        queue = queue + (i['callees']||[]).map { |n| dstfunmap[n] or raise Exception.new }
      end
    end
  end
rescue Exception
  data['machine-functions'].each { |f2|
    next if %{_start abort _exit}.include?(f2['mapsto'])
    called.add(f2['name'])
  }
end
puts "Called Functions: #{called.inspect}"

# For all relation graphs
data['relation-graphs'].each do |rg|
  next unless called.include?(rg['dst']['function'])
  constraints = ConstraintSystem.new
  puts "<<< #{rg['src'].inspect}"
  puts ">>> #{rg['dst'].inspect}"
  # first, add predecessors information
  nodes = rg['nodes']
  nodes.each do |n1|
    (n1['src-successors']||=[]).each do |n2|
      (nodes[n2]['src-predecessors'] ||= []) << n1['name']
    end
    (n1['dst-successors']||=[]).each do |n2|
      (nodes[n2]['dst-predecessors'] ||= []) << n1['name']
    end
  end

  # helpers to simplify specification
  $lab = NodeLabeller.new(rg)
  $constraints=constraints
  def eq(lhs,rhs); $constraints.eqs.add(lhs.add_lc(rhs.negate)); end
  def ins(node,ty) ; LinearCombination.new(node["#{ty}-predecessors"].map { |n| "#{$lab.label_at(n,ty)}->#{$lab.label(node,ty)}" }) ; end
  def out(node,ty) ; LinearCombination.new(node["#{ty}-successors"].map { |n| "#{$lab.label(node,ty)}->#{$lab.label_at(n,ty)}" }) ; end
  def scope(ty) ; LinearCombination.new([$lab.scope(ty)]) ; end

  # (1) for src nodes, inSCR = outSRC
  # (2) for dst nodes, inDST = outDST
  # (3) for progress nodes, (1),(2),inSRC=inDST,outSRC=outDST
  # (4) for entry nodes, outSRC = outDST = 1
  # (5) for exit nodes, inSRC = inDST = 1
  nodes.each do |node|
    if node['type'] == 'entry'
      eq(out(node,'src'),scope('src'))
      eq(out(node,'dst'),scope('dst'))
      eq(out(node,'src'),out(node,'dst'))
    elsif node['type'] == 'exit'
      eq(ins(node,'src'),scope('src'))
      eq(ins(node,'dst'),scope('dst'))
    else
      eq(ins(node,'src'),out(node,'src')) if node['type'] != 'dst'
      eq(ins(node,'dst'),out(node,'dst')) if node['type'] != 'src'
      eq(ins(node,'src'),ins(node,'dst')) if node['type'] == 'progress'
      eq(out(node,'src'),out(node,'dst')) if node['type'] == 'progress'
    end
  end
  constraints.dump
end
# Dump flow facts
ffs = {}
(data['flowfacts']||[]).each do |ff|
  (ffs[ff['scope']]||=[]).push(ff)
end
ffs.each do |scope,facts|
  constraints = ConstraintSystem.new
  puts "<> #{scope}"
  facts.each do |ff|
    lc = LinearCombination.new
    ff['lhs'].each do |t|
      puts t.inspect
      pp = t['program-point']
      ty = (ff['level']=='bitcode') ? 'src' : 'dst'
      k = "#{pp['function']}::#{pp['block']}"
      k = "#{ty}::#{k}"
      lc.add_term([k,t['factor']])
    end
    lc.add_term([nil,-ff['rhs']])
    if ff['op'] == "less-equal"
      constraints.ineqs.add(lc)
    else
      constraints.eqs.add(lc)
    end
  end
  constraints.dump
end
# guassian eliminate for machine code

