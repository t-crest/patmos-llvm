#!/usr/bin/env ruby
#
# PSK tool set
#
# Converts SWEET .ff files to PML format
#
# TODO: support a larger variety of flow facts (call strings, loop contexts)
require 'platin'
include PML
require 'ext/sweet'

# Exception for unsupported flow facts
class UnsupportedFlowFactException < Exception
  attr_reader :flowfact
  def initialize(msg, ff = nil)
    super(msg)
    @flowfact = ff
  end
end

# class to import SWEET flow facts (format .ff) to PML
class SweetFlowFactImport

  def initialize(functions, fact_context)
    @functions = functions
    @fact_context = fact_context
  end

  def to_pml(ffsrc)
    raise UnsupportedFlowFactException.new("loop scopes not yet supported", ffsrc) if ffsrc.quantifier != :total
    raise UnsupportedFlowFactException.new("loop scopes not yet supported", ffsrc) if ffsrc.scope.stmt
    raise UnsupportedFlowFactException.new("call strings not yet supported", ffsrc) unless ffsrc.callstring.empty?
    scope = @functions.by_name(ffsrc.scope.f)
    terms = ffsrc.constraint.vector.map { |pp,factor|
      Term.new(pp_to_pml(pp).ref, factor)
    }
    op =
      case ffsrc.constraint.op
      when "<="; "less-equal"
      when "=" ; "equal"
      else     ; raise Exception.new("Bad constraint op: #{ffsrc.constraint.op}")
      end
    flowfact = FlowFact.new(scope.ref, TermList.new(terms), op, ffsrc.constraint.rhs)
    flowfact.add_attributes(@fact_context)
    flowfact
  end

  def pp_to_pml(pp)
    raise UnsupportedFlowFactException.new("edge program points not yet supported") if pp.kind_of?(SWEET::Edge)
    llvm,internal = pp.split(":::")
    fun,block,ins = llvm.split("::")
    # For upper bounds, we could ignore the internal structure of the block
    raise UnsupportedFlowFactException.new("translation internal program points not supported") if internal
    raise UnsupportedFlowFactException.new("instruction program points not supported") if ins
    @functions.by_name(fun).blocks.by_name(block)
  end

end

class SweetImportTool
  def SweetImportTool.add_options(opts,exclude=[])
    opts.generates_flowfacts
    opts.sweet_flowfact_file unless exclude.include?(:sweet_flowfact_file)
  end

  def SweetImportTool.run(pml, options)
    parser = SWEET::FlowFactParser.new.parser
    converter = SweetFlowFactImport.new(pml.bitcode_functions, 'level' => 'bitcode',
                                        'origin' => options.flow_fact_output || 'sweet')
    ffs = []
    added, skipped, reasons, set = 0,0, Hash.new(0), {}
    File.readlines(options.sweet_flowfact_file).map do |s|
      begin
        ff = parser.parse!(s)
        ff_pml = converter.to_pml(ff)
        if set[ff_pml]
          reasons["duplicate"] += 1
          skipped+=1
        else
          set[ff_pml] = true
          pml.flowfacts.add(ff_pml)
          added += 1
        end
      rescue UnsupportedFlowFactException=>detail
        reasons[detail.to_s] += 1
        skipped += 1
      end
    end
    statistics("added flow facts" => added, "skipped flow facts" => skipped) if options.stats
    if options.verbose
      $dbgs.puts "Reasons for skipping flow facts: "
      reasons.each do |k,count|
        $dbgs.puts "  #{k} (#{count})"
      end
    end
    pml
  end

end


if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Translate SWEET flow facts (format FF) to PML flow facts
EOF
  options, args = PML::optparse([:sweet_flowfact_file], "file.ff", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    SweetImportTool.add_options(opts, [:sweet_flowfact_file])
  end
  SweetImportTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
