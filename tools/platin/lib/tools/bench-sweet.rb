#!/usr/bin/env ruby
#
require 'platin'
include PML

require 'platin/tools/analyze-trace'
require 'platin/tools/extract-symbols'
require 'platin/tools/pml2ais'
require 'platin/tools/ait2pml'
require 'platin/tools/ff2pml'
require 'platin/tools/transform'
require 'platin/tools/wca'
require 'platin/tools/sweet'

class BenchToolSweet
  TOOLS = [SweetAnalyzeTool, SweetTraceTool, SweetImportTool,
           ExtractSymbolsTool,AnalyzeTraceTool, RelationGraphValidationTool,
           RelationGraphTransformTool, WcaTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool]
  def BenchToolSweet.step(descr)
    $stderr.puts("+ #{descr}")
    t1 = Time.now
    yield
    t2 = Time.now
    $stderr.puts("- #{descr.ljust(35)} #{((t2-t1)*1000).to_i} ms")
  end
  def BenchToolSweet.run(pml,options)

    step("Extracting Addresses")           { ExtractSymbolsTool.run(pml,options).dump_to_file(options.input) }

    step("Running SWEET analysis analysis and trace generation")  {
      opts = options.dup
      opts.sweet_generate_trace = true
      SweetAnalyzeTool.run(pml, opts)
    }
    step("Import SWEET flow facts")        { SweetImportTool.run(pml, options) }

    # XXX: dump_to_file: during dev only
    step("Analyze MC Traces")              { AnalyzeTraceTool.run(pml,options).dump_to_file(options.input) }

    step("Validate Relation Graph")        {
      begin
        RelationGraphValidationTool.run(pml,options)
      rescue Exception => detail
        $stderr.puts ("RG Validation failed: #{detail}")
      end
    }

    step("Running WCA (machine code - trace facts)")  {
      opts = options.dup
      opts.flow_fact_srcs = ["trace"]
      opts.flow_fact_selection = "all"
      opts.timing_name = "wca-trace-all"
      WcaTool.run(pml, opts)
    }

    step("Running WCA (machine code - minimal trace facts)") {
      opts = options.dup
      opts.flow_fact_selection = "minimal"
      opts.timing_name = "wca-trace-minimal"
      WcaTool.run(pml,opts)
    }

    step("Copy trace flow facts which are cannnot covered by SWEET facts (compiler-rt)") {
      opts = options.dup
      opts.transform_action = "copy"
      opts.flow_fact_srcs   = ["trace"]
      opts.flow_fact_selection  = "rt-support"
      opts.flow_fact_output = "trace.support"
      RelationGraphTransformTool.run(pml,opts)
    }

    step("Running WCA (relation graph - SWEET facts)")   {
      opts = options.dup
      opts.flow_fact_selection = "all"
      opts.flow_fact_srcs = ["sweet","trace.support"]
      opts.use_relation_graph = true
      opts.timing_name = "wca-sweet-all"
      WcaTool.run(pml, opts)
    }

    # not yet supported
    #options.flow_fact_selection = "minimal"
    #options.timing_name = "wca-sweet-minimal"
    #step("Running WCA (relation graph - minimal SWEET facts)")   { WcaTool.run(pml, options) }

    step("AIS Export")  {
      opts = options.dup
      opts.flow_fact_selection = "all"
      opts.flow_fact_srcs = "all"
      opts.timing_name = "aiT-minimal"
      AisExportTool.run(pml,opts)
      ApxExportTool.run(pml,opts)
    }

    step("Run aiT")                        { AitAnalyzeTool.run(pml, options) }
    step("Import aiT Results")             { AitImportTool.run(pml,options) }

    step("Results: ") {
      pml.data['timing'].each do |t|
        puts YAML::dump(t)
      end
    }
    pml
  end
  def BenchToolSweet.add_options(opts)
    opts.writes_pml
    TOOLS.each { |toolclass| toolclass.add_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT"
EOF
  options, args = PML::optparse([:input], "program.elf.pml", SYNOPSIS) do |opts|
    BenchToolSweet.add_options(opts)
  end
  BenchToolSweet.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
