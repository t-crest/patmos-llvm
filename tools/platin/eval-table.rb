#
# platin tool set
#
#  build a table from collected evaluation results
#
require 'find'
require 'yaml'
require 'json'
require 'core/pml'
require 'set'
require 'pp'
include PML

if ARGV.length == 0
  $stderr.puts "Usage: #{$0} results.json"
  exit 1
end
json = File.read(ARGV.first)
db = JSON.parse(json)

# formatters
class Numeric
  def to_percent(prec)
    sprintf("%.#{prec}f%%",self*100.0)
  end
  def to_str(prec)
    sprintf("%.#{prec}f",self)
  end
end
def units(v)
  if(v.to_f >= 100000000)
    "#{v/1000000}M"
  elsif (v.to_f >= 100000)
    "#{v/1000}K"
  else
    "#{v}"
  end
end
def relative_change(v,base,prec=2)
  ((v.to_f-base.to_f)/base.to_f).to_percent(prec)
end
def relative(v,base,prec=2)
  (v.to_f/base.to_f)
end

WIDTH=13
headers=%w{benchmark V_bc V_mc u(V_mc) V_rg S_cfrg/S_cfg T_mc T_rg}

# extract all entries of interest
def values(e)
  Enumerator.new do |y|
    y << e['mod']
    y << e['metric-bcblocks']
    y << e['metric-mcblocks']
    y << relative(e['metric-mcblocks-unmapped'],e['metric-mcblocks']).to_percent(1)
    y << e['metric-rgblocks']
    y << relative(e['metric-rgblocks']+e['metric-rgprogress'],e['metric-mcblocks']+e['metric-bcblocks']).to_str(2)
    # y << e['metric-rgdst']
    y << units(e['wca-trace-all'])
    y << relative_change(e['wca-sweet-all'],e['wca-trace-all'],3)
    # y << relative_change(e['wca-uptrace-all-size'],e['wca-trace-all-size'],1)
  end
end

data = db['data']
# filter
data = data.select { |e| e['arch'] == 'patmos' && e['opt'] == 'OPT-2' && e['mod'] != 'jumptable2' }
# sort
data = data.sort_by { |e| [e['wca-trace-all'].to_i > 10 ? 0 : 1, e['mod'] ] }

puts headers.map { |s| s.to_s.ljust(WIDTH) }.join(" & ") + "\\\\"
puts "\\hline"
data.each { |d|
  puts values(d).map { |s| s.to_s.ljust(WIDTH) }.join(" & ") + "\\\\"
}
