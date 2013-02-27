#
# PSK Toolchain
#
# Relation Graph utilities: validate, flow fact transformation
#

require 'platin'
require 'ext/sweet'
include PML

class RelationGraphValidation
  SHOW_ERROR_TRACE=10
  def initialize(pml, options)
    @pml, @options = pml, options
  end
  def validate(pt1, pt2)
    tsrc, tdst = pt1.trace, pt2.trace
    ix_src, ix_dst = 0,0
    while ix_src < tsrc.length && ix_dst < tdst.length
      while is_machine_only_node(tdst[ix_dst],tsrc[ix_src])
        ix_dst+=1
        if ix_dst == tdst.length
          raise Exception.new("RelationGraphValidation failed: ran out of MC entries at #{tsrc[ix1]}")
        end
      end
      p1, p2 = tsrc[ix_src], tdst[ix_dst]
      if p1!= p2
        if SHOW_ERROR_TRACE > 0
          info("Progress Trace Validation Mismatch")
          info("Trace to SRC:")
          (-SHOW_ERROR_TRACE..SHOW_ERROR_TRACE).each do |off|
            is,id = [0,ix_src+off].max, [0,ix_dst+off].max
            pt1.internal_preds[is].each { |n|
              $stderr.puts "        #{n}"
            }
            pt2.internal_preds[id].each { |n|
              $stderr.puts "        #{" "*30} #{n}"
            }
            $stderr.puts "    #{off.to_s.rjust(3)} #{tsrc[is].to_s.ljust(30)} #{tdst[is]}"
          end
        end
        raise Exception.new("Progress trace validation failed: #{p1} != #{p2}") if p1 != p2
      end
      #$dbgs.puts "Match: #{p2.data.inspect}" if @options.debug
      ix_src,ix_dst = ix_src+1, ix_dst+1
    end

    statistics("progress trace length (src)" => pt1.trace.length,
               "progress trace length (dst)" => pt2.trace.length) if @options.stats
  end
  def is_machine_only_node(dstnode, srcnode)
    return false if dstnode.rg == srcnode.rg
    return @pml.machine_code_only_functions.include?(dstnode.get_block(:dst).function.label)
  end
end

class RelationGraphValidationTool
  def RelationGraphValidationTool.add_options(opts, mandatory = true)
    opts.analysis_entry
    opts.binary_file(mandatory)
    opts.pasim
    opts.sweet_trace_file(mandatory)
  end
  def RelationGraphValidationTool.check_options(options)
    die_usage("Binary file is needed for validation. Try --help") unless options.binary_file
    die_usage("SWEET trace file is needed for validation. Try --help") unless options.sweet_trace_file
  end

  def RelationGraphValidationTool.run(pml, options)
    mtrace = pml.arch.simulator_trace(options)
    tm1 = MachineTraceMonitor.new(pml, mtrace)
    entry  = pml.machine_functions.by_label(options.analysis_entry)
    pt1 = ProgressTraceRecorder.new(pml, entry, true, options)
    tm1.subscribe(pt1)
    tm1.run

    tm2 = SWEET::TraceMonitor.new(options.sweet_trace_file, pml)
    pt2 = ProgressTraceRecorder.new(pml, options.analysis_entry, false, options)
    tm2.subscribe(pt2)
    tm2.run
    RelationGraphValidation.new(pml,options).validate(pt2, pt1)
  end
end

class RelationGraphTransformTool
  TRANSFORM_ACTIONS=%w{up down copy}
  SUPPORTED_FLOW_FACT_TYPES=%w{loop-local loop-function loop-global block-local block-function block-global infeasible-global calltargets-global}

  def RelationGraphTransformTool.add_options(opts)
    opts.analysis_entry
    opts.flow_fact_selection
    opts.generates_flowfacts
    opts.on("--validate", "Validate relation graph") { opts.options.validate = true }
    opts.on("--transform-action ACTION", "action to perform (=down,up,copy)") { |action| opts.options.transform_action = action }
    RelationGraphValidationTool.add_options(opts, false)
    opts.add_check { |options|
      die_usage("Missing option --transform-action") unless options.transform_action
      die_usage("Bad action #{options.transform_action} (not in {#{TRANSFORM_ACTIONS.join(",")}})") unless TRANSFORM_ACTIONS.include?(options.transform_action)
      if options.validate
        RelationGraphValidationTool.check_options(options)
      end
    }
  end

  # pml ... PML for the prgoam
  def RelationGraphTransformTool.run(pml,options)
    #require 'perftools'
    #PerfTools::CpuProfiler.start("/tmp/platin-transform")

    if(options.validate)
      RelationGraphValidationTool.run(pml,options)
    end

    # Analysis Entry
    entry = pml.machine_functions.by_label(options.analysis_entry)

    # flow facts
    flowfacts = pml.flowfacts.filter(pml, options.flow_fact_selection, options.flow_fact_srcs,  ["bitcode","machinecode"])

    fft = FlowFactTransformation.new(pml,options)
    if options.transform_action == "copy"
      fft.copy(flowfacts)
    elsif options.transform_action == "up" || options.transform_action == "down"
      dir = options.transform_action == "up" ? :src : :dst
      new_ffs = fft.transform(entry, flowfacts, dir)
      new_ffs.each { |ff|
        ff.add_attribute('origin', options.flow_fact_output)
        ff.add_attribute('level', (dir == :src) ? "bitcode" : "machinecode")
        pml.flowfacts.add(ff)
      }
      info("Added #{new_ffs.length} transformed flowfacts to #{options.flow_fact_output}")
    else
      die("Bad transformation action --transform-action=#{options.transform_action}")
    end
    #PerfTools::CpuProfiler.stop
    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF
Validates relation graph and transforms flow facts from IR level to machine code level.
EOF
  options, args = PML::optparse(0, "", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    RelationGraphTransformTool.add_options(opts)
  end
  RelationGraphTransformTool.run(PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
