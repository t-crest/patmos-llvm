require 'platin'
include PML

class PMLPath
  attr_reader :ftype, :fpred, :ipred
  def initialize(str)
    @str = str

    # function type
    str =~ /^\/(bc|mc|\*)/
    @ftype, str = $1, $'

    # predicate(s) for selection
    @predicates = []
    while str=~ /^\/(?<p>\*|\[.+?\])/
      @predicates.push(parse_predicate($1))
      str = $'
    end
    warn ("Trailing unparsed PMLPath: #{str}") unless str.empty?
  end
  def parse_predicate(str)
    return :all if str == '*'
    m = /^\[@(.+?)=(.+)\]$/.match(str)
    raise Exception.new("Bad predicate in PMLpath: '#{str}'") unless m
    [m[1],m[2]]
  end
  def lvl_to_num(lvl)
    { :func => 0, :bb => 1, :inst => 2 }[lvl]
  end
  def has_level?(lvl)
    lvl = lvl_to_num(lvl) unless lvl.class == Fixnum
    lvl && lvl < @predicates.size
  end
  def pred_at_level(lvl)
    lvl = lvl_to_num(lvl) unless lvl.class == Fixnum
    lvl ? @predicates[lvl] : nil
  end
  def to_s
    @str
  end
end

class PMLMatchModify
  attr_reader :path, :action
  def initialize(path, options, action=nil)
    @path, @options  = path, options
    if action == nil || action.is_a?(Symbol)
      @action = action
    else
      # argument is tuple/array
      @action, @target, @replacement = action
    end
  end

  def match_functions(ff, pred)
    return ff if pred == :all
    return [] if ff.empty? # we peak into the 1st function to figure out the label key
    pred_attr = pred[0] == "label" ? ff.first.labelkey : pred[0]
    ff.select { |f| f.send(pred_attr).to_s == pred[1] }
  end

  def match_other(oo, pred)
    return oo if pred == :all
    oo.select { |o| match_object(o, pred[0], Regexp.new(pred[1])) != nil }
  end

  def match_object(pml_object, attr_name, rx)
    attrib = pml_object.send(attr_name)
    match_attrib(attrib, rx)
  end
  def match_attrib(attrib, rx)
    if attrib.class == Array
      m = attrib.map { |a| match_attrib(a,rx) }.compact
      return nil if m == []
    else
      m = rx.match attrib.to_s
    end
    m
  end

  def modify(pml_object, action)
    unless [:clear].include? action
      warn ("Unknown PML modification action: #{action.to_s}")
      return
    end
    data = pml_object.data
    nummod = 0
    case action
    when :clear
      (pml_object.data[@target] = nil; nummod += 1) if data.include? @target
    end
    puts "#{pml_object}: #{nummod} modification(s)" if nummod > 0
  end
end

class PML::Function
  attr_reader :labelkey
end

class PMLDoc
  def match_path(matcher)
    ppath = matcher.path
    case ppath.ftype
    when "bc"
      ft = [@bitcode_functions]
    when "mc"
      ft = [@machine_functions]
    when "*"
      ft = [@bitcode_functions, @machine_functions]
    else
      raise Exception.new("bad function type: #{ppath.ftype}")
    end

    fs = ft.map { |ff| matcher.match_functions(ff.list, ppath.pred_at_level(:func)) }.flatten
    if ppath.has_level? :bb
      bb = fs.map { |f| matcher.match_other(f.blocks.list, ppath.pred_at_level(:bb)) }.flatten
    end
    if ppath.has_level? :inst
      ii = bb.map { |b| matcher.match_other(b.instructions.list, ppath.pred_at_level(:inst)) }.flatten
      ii.each { |i| matcher.modify(i, matcher.action) } if matcher.action
    end
    ii || bb || fs
  end

end

class PMLModTool
  attr_reader :pml, :options
  def initialize(pml, options)
    @pml, @options = pml, options
  end

  def PMLModTool.add_options(opts)
    opts.writes_pml
    opts.on("--match PMLPATH", "match instructions via PMLPath") { |p| opts.options.pml_path = p }
    opts.on("--clear-callees", "remove callees from matched instructions") { |p| opts.options.clear_callees = true }
  end

  def PMLModTool.run(pml, options)
    tool = PMLModTool.new(pml, options)

    if options.clear_callees
      action = [:clear,'callees']
    end

    matcher = PMLMatchModify.new(PMLPath.new(options.pml_path), options, action)

    # run match/modification
    results = pml.match_path(matcher)

    # just matching, print results
    unless action
      puts "Matching (#{matcher.path}):"
      results.each_with_index { |o,i| puts "#{i}: #{o}" }
      puts '-end-'
    end

    #matcher = PMLMatchModify.new(PMLPath.new(x['path']), @options, x['action'] || nil)
    pml
  end
end

SYNOPSIS=<<EOF
Programmatically modify PML documents.
EOF

if __FILE__ == $0
  options, args = PML::optparse([], "", SYNOPSIS) do |opts|
    opts.needs_pml
    PMLModTool.add_options(opts)
  end
  updated_pml = PMLModTool.run(PMLDoc.from_files(options.input), options)
  updated_pml.dump_to_file(options.output) if options.output
end

