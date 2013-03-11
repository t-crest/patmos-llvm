#
# PLATIN tool set
#
# eval-summarize
# Simple script to summarize evaluation results (timing entries)
#

require 'platin'
require 'core/utils'
include PML

###################################################################################################
# Customize Below
###################################################################################################

# title
title = "PLATIN evaluation: trace flow-facts + platin-wca + platin-transform + aiT"

# the tming entry we compare everything else against
baseline = "trace"

# correction term for baseline measurement
overhead = -1

# columns (in order), and labels for columns
column_labels = [ [ 'name'      , 'Name' ],
                  [ 'metric-functions' , 'F' ],
                  [ 'metric-bcblocks'  , 'V_ir' ],
                  [ 'metric-mcblocks'  , 'V_mc' ],
                  [ 'metric-rgblocks'  , 'V_rel' ],
                  [ 'metric-rtblocks'  , 'V_asm' ],
                  [ 'trace'             , 'trace' ],
                  [ 'wca-trace-all'     , 'wca/tr' ],
                  [ 'wca-uptrace-all'   , 'wca/tr-rg' ],
                  [ 'aiT-trace-all'     , 'aiT/tr' ],
                  [ 'wca-sweet-all'     , 'wca/sweet' ],
                  [ 'wca-trace-local'   , 'wca/tr/local' ],
                  [ 'wca-uptrace-local' , 'wca/tr-rg/local' ],
                  [ 'aiT-trace-local'   , 'aiT/tr/local' ],
                  [ 'wca-sweet-local'   , 'wca/sweet/local' ],
                  [ 'wca-trace-minimal' , 'wca/tr/min' ],
                  [ 'wca-uptrace-minimal', 'wca/tr-rg/min' ],
                  [ 'aiT-trace-minimal' , 'aiT/tr/min' ],
                  [ 'wca-sweet-minimal', 'wca/sweet/min' ] ]
# legend
legend = <<EOF
name ... Name of the benchmark

F     ... Number of functions in the analyzed part of the program
V_ir  ... IR-level basic blocks
V_mc  ... MC-level basic blocks (without compiler-rt)
V_rg  ... Number of Relation Graph Nodes
V_asm ... MC-level basic blocks (compiler-rt)

trace     ... Cycles to execute benchmark using *pasim* (simple timing model)
wca/      ... WCET-estimate calculated using platin-wca
aiT/      ... WCET-estimate calculated using a3patmos
*/tr      ... Flow facts generated from trace (valid for benchmark run only)
*/tr-rg   ... 'tr' flow facts transformed to bitcode level using relation graphs
*/*/local ... Only use intraprocedural flow facts
*/*/min   ... Only use flow facts corresponding to (a) loop bounds (b) infeasible blocks
              and (c) indirect call targets
EOF

###################################################################################################

# Simple Table class to collect and render evaluation results
class Table
  attr_reader :colopts
  def initialize
    @rows, @cols, @colopts = [], [], {}
  end
  def add_column(name, opts = {})
    raise Exception.new("Duplicate column #{name}") if @colopts[name]
    @cols.push(name)
    @colopts[name] = opts
    @colopts[name][:label] = name unless opts[:label]
  end
  def ensure_column(name, opts = {})
    add_column(name, opts) unless @colopts[name]
  end
  def remove_empty_columns!
    newcols = []
    @cols.each do |col|
      if @rows.any? { |row| row.data[col] }
        newcols.push(col)
      else
        @colopts.delete(col)
      end
    end
    @cols = newcols
  end
  class Row
    attr_reader :data, :rowopts, :cellopts
    def initialize(table, rowopts)
      @table = table
      @rowopts = rowopts
      @data,@cellopts = {},Hash.new { Hash.new }
    end
    def add(key, value, opts = {})
      @table.ensure_column(key, opts)
      @data[key] = value
      @cellopts[key] = opts.dup
      @cellopts[key][:align] ||= @table.colopts[key][:align]
    end
  end
  def add_row(rowopts = {})
    r = Row.new(self, rowopts)
    @rows.push(r)
    r
  end

  # returns table data in YAML format
  def to_yaml(name)
    data = @rows.map { |r| r.data }
    YAML::dump([ { 'name' => name, 'data' => data } ])
  end

  # dumps table to @io@
  #  needs a block that takes [row,col], and returns [text, color]
  def dump(io = $stdout)
    colwidth = Hash[@cols.map { |c| [c, @colopts[c][:label].length] }]
    data = @rows.map { |r|
      rowdata = {}
      r.data.each { |col,v|
        txt, color = yield [r,col]
        width = txt.length
        colwidth[col] = [colwidth[col], width].max
        rowdata[col] = [ txt, color ]
      }
      [r, rowdata]
    }
    io.puts(@cols.map { |c| @colopts[c][:label].ljust(colwidth[c]) }.join(" | "))
    io.puts(@cols.map { |c| "-" * colwidth[c] }.join("-|-"))
    data.each { |row,dat|
      rowtext = @cols.map { |c|
        txt, color = dat[c]
        format_text(txt || "--", :width => colwidth[c], :color => color,
                    :align => row.cellopts[c][:align] || @colopts[c][:align] )
      }.join(" | ")
      io.puts(rowtext)
    }
  end

  # return table description in html format
  # FIXME: this is just a very quick hack
  def to_html(name)
    s = "<h2>Evaluation Results: #{name}</h2>\n"
    s << "<table style=\"font-size: smaller;\" border=\"1\">\n"
    s << "  <tr>"
    @cols.each { |c|
      s << "<th>#{@colopts[c][:label]}</th> "
    }
    s << "</tr>\n"
    @rows.each { |r|
      s << "  <tr>"
      @cols.each { |c|
        repr = yield [r,c]
        s << "#{repr}"
      }
      s << "</tr>\n"
    }
    s << "</table>"
    s
  end

  private
  def format_text(txt, opts)
    if opts[:align] == :right
      txt = txt.to_s.rjust(opts[:width])
    else
      txt = txt.to_s.ljust(opts[:width])
    end
    if color = opts[:color]
      txt = colorize_text(txt, color)
    end
    txt
  end
  def colorize_text(txt, color)
    return txt unless color
    colors = %w{black red green yellow blue magenta cyan white}
    fg = Hash[colors.zip(30..37)]
    bg = Hash[colors.zip(40..47)]
    fg_mod = "\033[#{fg[color]}m"
    "#{fg_mod}#{txt}\033[0m"
  end
end


if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Summarize evaluation results
EOF

  options, args = PML::optparse(nil, "[file.pml...]", SYNOPSIS) do |opts|
    opts.on("--name NAME","name of the evaluation result set") { |n| opts.options.result_name = n }
    opts.on("--html FILE","write HTML report to this file") { |f| opts.options.html = f }
    opts.on("--logdir DIR", "directory containing benchmark run reports") { |d| opts.options.logdir = d }
    opts.on("--html-prolog","write HTML prolog and exit") { opts.options.html_prolog = true }
    opts.on("--html-epilog","write HTML epilog and exit")   { opts.options.html_epilog = true }
  end
  args = [ $< ] if args.empty?
  if options.html_prolog
    File.open(options.html, "w") { |fh|
      fh.puts("<html><head><title>#{title}</title></head><body><h1>#{title}</h1>")
    }
    exit 0
  elsif options.html_epilog
    File.open(options.html, "a") { |fh|
      fh.puts("<h2>Legend</h2><pre>\n#{legend}</pre>")
      fh.puts("</body></html>")
    }
    exit 0
  end

  tab = Table.new
  column_labels.each { |n,lab| tab.add_column(n, :label => lab, :align => (n=="name") ? :left : :right) }

  ARGV.each do |fn|
    name = File.basename(fn,'.pml')
    values = {}
    File.open(fn) { |fh|
      pml = PMLDoc.new(fh)
      row = tab.add_row
      row.add("name",name)

      entry_bc, entry_mc = "main", pml.machine_functions.by_label("main").name
      fs_bc = pml.bitcode_functions.reachable_from("main").first
      fs_mc = pml.machine_functions.reachable_from(entry_mc).first
      fs_mc_mapped = fs_mc.select { |f| fs_bc.any? { |fbc| fbc.name == f.label } }
      fs_mc_unmapped = fs_mc.reject { |f| fs_mc_mapped.include?(f) }

      v_bc = fs_bc.map { |f| f.blocks.length }.inject(0,:+)
      v_mc = fs_mc_mapped.map { |f| f.blocks.length }.inject(0,:+)
      v_mc_unmapped = fs_mc_unmapped.map { |f| f.blocks.length }.inject(0,:+)
      v_rg = fs_bc.map { |f| pml.relation_graphs.by_name(f.name, :src).nodes.length }.inject(0,:+)

      row.add("metric-functions",fs_bc.length)
      row.add("metric-bcblocks",v_bc)
      row.add('metric-mcblocks',v_mc)
      row.add('metric-rtblocks',v_mc_unmapped)
      row.add('metric-rgblocks',v_rg)

      pml.timing.each { |te|
        if row.data[te.origin]
          $stderr.puts("Skipping duplicate entry #{te}")
          next
        end
        value = te.cycles
        value = nil if te.cycles <= 0
        row.add(te.origin, value)
      }
    }
  end

  tab.remove_empty_columns!

  # returns [relative_value, relative_value_repr] or nil
  def relative_change(value,base)
    return nil if ! base || ! value || value <= 0
    rel = ((value.to_f/base) - 1) * 100
    return [0, "0"] if rel==0
    return [rel, sprintf("%s%.2f\%",rel>0?"+":"-",rel.abs)]
  end

  # returns [text, category] for a cell in the table
  def format_cell(row, col, baseline, overhead, relative = true)
    base_cycles = (row.data[baseline] || 0) - overhead
    raw = row.data[col]
    if ! raw
      [ nil, 'missing' ]
    elsif col == baseline
      [ base_cycles, 'baseline' ]
    elsif col =~ /(\w+)-(\w+)-(\w+)/
      set,tool,selection = $1,$2,$3
      value,repr = relative_change(raw, base_cycles)
      repr = raw.to_s unless relative
      return [ nil, 'invalid' ] unless value
      if value < 0
        [ repr, 'underestimation' ]
      elsif value > 100
        [ repr, 'imprecise' ]
      else
        [ repr, 'ok' ]
      end
    else
      [ raw.to_s, 'metric' ]
    end
  end

  if options.verbose
    colors = { 'invalid' => 'cyan', 'missing' => 'cyan', 'underestimation' => 'red',
      'valid' => 'green', 'imprecise' => 'yellow' }
    tab.dump($stderr) { |row,col|
      txt, cat = format_cell(row, col, baseline, overhead)
      [ (txt || "--").to_s, colors[cat] ]
    }
  end
  if options.html
    File.open(options.html,"a") { |fh|
      r = tab.to_html(options.result_name) { |row,col|
        if col == 'name'
          name = row.data['name']
          "<td><a href=\"#{File.join(options.logdir,name+".log")}\">#{name}</a></td>"
        else
          txt, cat = format_cell(row, col, baseline, overhead)
          bg = case cat
               when 'invalid' then 'cyan'
               when 'missing' then 'cyan'
               when 'underestimation' then 'red'
               when 'imprecise' then 'yellow'
               when 'ok' then 'lightgreen'
               else 'white'
               end
          "<td style=\"background-color: #{bg}; padding: 3pt; text-align: right\">#{txt || '&nbsp;' }</td>"
        end
      }
      fh.puts(r)
    }
  end
  puts tab.to_yaml(options.result_name)
end # main
