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

class SweetImportTool
  def SweetImportTool.add_config_options(opts)
  end
  def SweetImportTool.add_options(opts,exclude=[])
    SweetImportTool.add_config_options(opts)
    opts.generates_flowfacts
    opts.sweet_flowfact_file unless exclude.include?(:sweet_flowfact_file)
  end

  def SweetImportTool.run(pml, options)
    parser = SWEET::FlowFactParser.new.parser
    flow_fact_origin = options.flow_fact_output || 'sweet'
    converter = SweetFlowFactImport.new(pml.bitcode_functions, 'level' => 'bitcode',
                                        'origin' => flow_fact_origin)
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
    statistics("SWEET", "added flow facts (=>#{flow_fact_origin})" => added, "skipped flow facts" => skipped) if options.stats
    debug(options, :sweet) { "Reasons for skipping flow facts: #{reasons.inspect}" }
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
  SweetImportTool.run(PMLDoc.from_files(options.input), options).dump_to_file(options.output)
end
