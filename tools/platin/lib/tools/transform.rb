#
# PSK Toolchain
#
# Relation Graph utilities: validate, flow fact transformation
#

require 'platin'
require 'ext/sweet'
include PML

# FIXME: it might happen that calls are moved to other nodes
# This is not a problem for our model, but validation fails (ndes)
class RelationGraphValidation
  SHOW_ERROR_TRACE=3
  def initialize(pml, options)
    @pml, @options = pml, options
  end
  def validate(pt1, pt2)
    tsrc, tdst = pt1.trace, pt2.trace
    errors = []
    # first, we extract a hierarchical trace
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
          info("Progress Trace Validation Mismatch: #{p1} vs #{p2}")
          info("Trace to SRC:")
          (-SHOW_ERROR_TRACE..SHOW_ERROR_TRACE).each do |off|
            is,id = [ [0,ix_src+off].max, tsrc.length - 1].min, [ [0,ix_dst+off].max, tdst.length - 1 ].min
            pt1.internal_preds[is].each { |n|
              $stderr.puts "        #{n}"
            }
            pt2.internal_preds[id].each { |n|
              $stderr.puts "        #{" "*30} #{n}"
            }
            $stderr.puts "    #{off.to_s.rjust(3)} #{tsrc[is].to_s.ljust(30)} #{tdst[id]}"
          end
        end
        # If we have an off by one error, we try to continue, collecting the errors
        if (p1 == tdst[ix_dst+1])
          errors.push([p1,p2])
          ix_dst+=1
        elsif (tsrc[ix_src+1] == p2)
          errors.push([p1,p2])
          ix_src+=1
        else
          raise Exception.new("Progress trace validation failed: #{p1} != #{p2}")
        end
      else
        ix_src,ix_dst = ix_src+1, ix_dst+1
      end
    end
    if ! errors.empty?
      raise Exception.new("Progress trace validation failed: #{errors.inspect}")
    end
    statistics("CFRG-VALIDATION",
               "progress trace length (src)" => pt1.trace.length,
               "progress trace length (dst)" => pt2.trace.length) if @options.stats
  end
  def is_machine_only_node(dstnode, srcnode)
    return false if dstnode.rg == srcnode.rg
    return @pml.machine_code_only_functions.include?(dstnode.get_block(:dst).function.label)
  end
end

class RelationGraphValidationTool
  def RelationGraphValidationTool.add_options(opts, mandatory = true)
    Architecture.simulator_options(opts)
    opts.analysis_entry
    opts.trace_entry
    opts.binary_file(mandatory)
    opts.sweet_trace_file(mandatory)
  end
  def RelationGraphValidationTool.check_options(options)
    die_usage("Binary file is needed for validation. Try --help") unless options.binary_file
    die_usage("SWEET trace file is needed for validation. Try --help") unless options.sweet_trace_file
  end

  def RelationGraphValidationTool.run(pml, options)
    mtrace = pml.arch.simulator_trace(options)
    tm1 = MachineTraceMonitor.new(pml, options, mtrace)
    entry = pml.machine_functions.by_label(options.analysis_entry)
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

class TransformTool
  TRANSFORM_ACTIONS=%w{up down copy}

  def TransformTool.add_options(opts)
    opts.analysis_entry
    opts.flow_fact_selection
    opts.on("--validate", "Validate relation graph") { opts.options.validate = true }
    opts.on("--transform-action ACTION", "action to perform (=down,up,copy,simplify)") { |action|
      opts.options.transform_action = action
    }
    opts.on("--transform-eliminate-edges", "eliminate edges in favor of blocks") {
      opts.options.transform_eliminated_edges = true
    }
    RelationGraphValidationTool.add_options(opts, false)
    opts.add_check { |options|
      if options.validate
        RelationGraphValidationTool.check_options(options)
      end
    }
  end

  # pml ... PML for the prgoam
  def TransformTool.run(pml,options)
    needs_options(options,:flow_fact_selection,:flow_fact_srcs,:transform_action,:analysis_entry)

    #require 'perftools'
    #PerfTools::CpuProfiler.start("/tmp/platin-transform")

    if(options.validate)
      RelationGraphValidationTool.run(pml,options)
    end

    # Analysis Entry
    entry = pml.machine_functions.by_label(options.analysis_entry)
    unless entry
      raise Exception.new("Analysis Entry #{options.analysis_entry} not found")
    end

    # Select flow facts
    flowfacts = pml.flowfacts.filter(pml, options.flow_fact_selection, options.flow_fact_srcs, ["bitcode","machinecode"])

    # resolve flow facts if possible
    # FIXME: we won't do this in FlowFactList
    add_flow_facts = self.resolve_symbolic_bounds(flowfacts)

    # Ignore symbolic loop bounds for now
    flowfacts.reject! { |ff| ff.symbolic_bound? }

    # Start transformation
    fft = FlowFactTransformation.new(pml,options)
    if options.transform_action == "copy"
      fft.copy(flowfacts)
    elsif options.transform_action == "up" || options.transform_action == "down"
      target_level = options.transform_action == "up" ? :src : :dst
      if flowfacts.any? { |ff| ff.level == (target_level == :dst ? "bitcode" : "machinecode") }
        fft.transform(entry, flowfacts, target_level)
      end
    elsif options.transform_action == "simplify"
      fft.simplify(entry, flowfacts)
    else
      die("Bad transformation action --transform-action=#{options.transform_action}")
    end
    #PerfTools::CpuProfiler.stop
    pml
  end

  # resolve triangle-loop recurrences where possible
  # XXX PLAYGROUND
  def TransformTool.resolve_symbolic_bounds(ffs)
    new_ffs = []
    by_loop = Hash.new
    dependencies = Hash.new
    ffs.each { |ff|
      s, b = ff.get_loop_bound
      if ff.symbolic_bound?
        ff.rhs.referenced_loops.each { |l| (dependencies[ff]||=Set.new).add(l) }
      end
      if s && s.context.empty?
        # if there is more than on loop bound, we should decide which one to take,
        # or instantiate multiple loop bounds. Not relevant in practice, probably.
        if by_loop[s.reference]
        end
        b = SEInt.new(b) if b.kind_of?(Integer)
        by_loop[s.reference] = b
      end
    }
    ffs.sort { |a,b|
      sa, ba = a.get_loop_bound
      sb, bb = b.get_loop_bound
      if sb && (dependencies[a]||[]).include?(sb.reference)
        1  # a depends on b and should be processed later
      elsif sa && (dependencies[b]||[]).include?(sa.reference)
        -1 # b depends on a and should be processed later
      else
        0
      end
    }.each { |ff,ix|

      s, b = ff.get_loop_bound
      next unless s && ff.symbolic_bound?

      refd_loops = b.referenced_loops
      next if refd_loops.empty?

      parent_loop = s.reference.loopblock.loops[1]
      if(refd_loops.size != 1 && refd_loops.first != parent_loop)
        warn "A loop different from the parent loop is referenced in a CHR: #{ff} -> #{refd_loops.inspect}"
        next
      end

      begin
        rb = b.resolve_loops(by_loop)
      rescue NoLoopBoundAvailableException => ex
        warn("Failed to resolve loop CHR, because outer loop bound for (#{ex.loop}) is not available")
        next
      end
      ff_new = FlowFact.loop_bound(s,rb,ff.attributes)
      info "CHR      loop bound: #{ff}"
      info "Resolved loop bound: #{ff_new}"
      if s && s.context.empty?
        by_loop[s.reference] = rb
      end
      if b.kind_of?(SEAffineRec)
        rbglob = b.global_bound(by_loop)
        ff_global = FlowFact.inner_loop_bound(ContextRef.new(refd_loops.first.loopref, s.context),
                                              ContextRef.new(s.reference, Context.empty),
                                              rbglob,
                                              ff.attributes)
        info "Triangle loop bound   : #{ff_global}"
      end
    }
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF
Transforms flow facts from IR level to machine code level or simplify set of flow facts
EOF
  options, args = PML::optparse(0, "", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    TransformTool.add_options(opts)
  end
  TransformTool.run(PMLDoc.from_files(options.input), options).dump_to_file(options.output)
end
