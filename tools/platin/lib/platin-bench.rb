#!/usr/bin/env ruby
#
require 'utils.rb'
include PML

require 'platin-analyze-trace'
require 'platin-extract-symbols'
require 'platin-pml2ais'
require 'platin-ait2pml'
require 'platin-ff2pml'
require 'platin-transform'
require 'platin-wca'


class BenchTool
  TOOLS = [ExtractSymbolsTool,AnalyzeTraceTool, RelationGraphValidationTool,
           WcaTool,
           AisExportTool,ApxExportTool,AitAnalyzeTool,AitImportTool]
  def BenchTool.step(descr)
    $stderr.puts("+ #{descr}")
    t1 = Time.now
    yield
    t2 = Time.now
    $stderr.puts("- #{descr.ljust(35)} #{((t2-t1)*1000).to_i} ms")
  end
  def BenchTool.run(pml,options)
    step("Extracting Addresses")           { ExtractSymbolsTool.run(pml,options).dump_to_file(options.input) }
    step("Analyze MC Traces")              { AnalyzeTraceTool.run(pml,options) }
    step("Up-transform MC Traces")         { TransformTool.run(pml,options.merge(:flow_fact_src="trace",:flow_fact_orgin=>"trace.bitcode")).
                                                           dump_to_file(options.input) } # XXX: during dev only
    step("Validate Relation Graph")        {
      begin
        RelationGraphValidationTool.run(pml,options)
      rescue Exception => detail
        $stderr.puts ("RG Validation failed: #{detail}")
      end
    }

    options.flow_fact_types = :supported
    options.flow_fact_srcs = ["trace"]
    options.timing_name = "platin-trace-all"
    step("Running WCA (machine code - trace facts)")     { WcaTool.run(pml, options) }
    options.flow_fact_types = FlowFact::MINIMAL_FLOWFACT_TYPES
    options.timing_name = "platin-trace-minimal"
    step("Running WCA (machine code - minimal trace facts)") { WcaTool.run(pml,options) }

    # copy trace flow facts which are cannnot covered by trace.bitcode facts (compiler-rt)
    # to trace.support
    copied = []
    pml.flowfacts.each { |ff|
      needed = ff.lhs.any? { |term| pml.machine_code_only_functions.include?(term.ppref.function.label) }
      if needed
        ff2 = ff.dup
        ff2.add_attribute('origin', 'trace.support')
        warn_once("Adding support for #{ff2.lhs.map { |t| t.ppref.function }.join(",")}")
        copied.push(ff2)
      end
    }
    copied.each { |ff| pml.flowfacts.add(ff) }
    options.flow_fact_types = :supported
    options.flow_fact_srcs = ["trace.bitcode","trace.support"]
    options.use_relation_graph = true
    options.timing_name = "platin-bitcode-all"
    step("Running WCA (relation graph - bitcode facts)")   { WcaTool.run(pml, options) }

    # not yet supported
    #options.flow_fact_types = FlowFact::MINIMAL_FLOWFACT_TYPES
    #options.timing_name = "platin-bitcode-minimal"
    #step("Running WCA (relation graph - minimal bitcode facts)")   { WcaTool.run(pml, options) }

    options.flow_fact_types = :supported
    options.flow_fact_srcs = "all"
    options.timing_name = "aiT-minimal"
    step("AIS Export")  {
      AisExportTool.run(pml,options)
      ApxExportTool.run(pml,options)
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
  def BenchTool.add_options(opts)
    opts.writes_pml
    TOOLS.each { |toolclass| toolclass.add_options(opts) }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
PSK benchmark run: extract symbols, analyze trace, run aiT"
EOF
  options, args = PML::optparse([:input], "program.elf.pml", SYNOPSIS) do |opts|
    BenchTool.add_options(opts)
  end
  BenchTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
