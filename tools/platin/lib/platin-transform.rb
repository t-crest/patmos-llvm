#
# PSK Toolchain
#
# Relation Graph utilities: validate, flow fact transformation
#

require 'utils'
require 'sweet'
require 'trace'
require 'ipet'
include PML

# Records progress node trace
class ProgressTraceRecorder
  attr_reader :level, :trace, :internal_preds
  def initialize(pml, entry, is_machine_code, options)
    @pml, @options = pml, options
    @trace, @entry, @level = [], entry, is_machine_code ? :dst : :src
    @internal_preds, @pred_list = [], []
    @callstack = []
  end
  # set current relation graph
  # if there is no relation graph, skip function
  def function(callee,callsite,cycles)
    @rg = @pml.relation_graphs.by_name(callee.name, @level)
    $dbgs.puts "Call to rg for #{@level}-#{callee}: #{@rg.nodes.first}" if @rg && @options.debug
    @callstack.push(@node)
    @node = nil
  end
  # follow relation graph, emit progress nodes
  def block(bb, _)
    return unless @rg
    if ! @node
      first_node = @rg.nodes.first
      assert("at_entry == at entry RG node") {
        first_node.type == :entry
      }
      assert("at_entry == at first block") {
        bb == first_node.get_block(level)
      }
      @node = first_node
      $dbgs.puts "Visiting first node: #{@node}" if @options.debug
      return
    end
    # find matching successor progress node
    succs = @node.successors_matching(bb, @level)
    if succs.length == 0
      raise Exception.new("progress trace: no matching successor")
    elsif succs.length > 1
      raise Exception.new("progress trace: indeterministic successor choice: #{@node} via #{bb}: #{succs}")
    else
      succ = succs.first
      if succ.type == :progress
        trace.push(succ)
        internal_preds.push(@pred_list)
        @pred_list = []
      else
        @pred_list.push(succ)
      end
      @node = succ
      $dbgs.puts "Visiting node: #{@node}" if @options.debug
    end
  end
  # set current relation graph
  def ret(rsite,csite,cycles)
    return if csite.nil?
    @rg = @pml.relation_graphs.by_name(csite.function.name, @level)
    @node = @callstack.pop
    $dbgs.puts "Return to rg for #{@level}-#{csite.function}: #{@rg.nodes.first}" if @rg and @options.debug
  end
  def eof ; end
  def method_missing(event, *args); end
end

class RelationGraphValidation
  SHOW_ERROR_TRACE=10
  def initialize(pml, options = OpenStruct.new(:debug => true))
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
    mtrace = SimulatorTrace.new(options.binary_file, options.pasim)
    tm1 = MachineTraceMonitor.new(pml, mtrace)
    entry  = pml.machine_functions.by_label(options.analysis_entry)
    pt1 = ProgressTraceRecorder.new(pml, entry, true, options)
    tm1.subscribe(pt1)
    tm1.run

    tm2 = SWEET::TraceMonitor.new(options.sweet_trace_file, pml)
    pt2 = ProgressTraceRecorder.new(pml, options.analysis_entry, false, options)
    tm2.subscribe(pt2)
    tm2.run
    RelationGraphValidation.new(pml).validate(pt2, pt1)
  end
end

class RelationGraphTransformTool
  SUPPORTED_FLOW_FACT_TYPES=%w{loop-local loop-function loop-global block-local block-function block-global infeasible-global calltargets-global}

  def RelationGraphTransformTool.add_options(opts)
    opts.analysis_entry
    opts.flow_fact_selection
    opts.generates_flowfacts
    opts.on("--validate", "Validate relation graph") { opts.options.validate = true }
    RelationGraphValidationTool.add_options(opts, false)
    opts.add_check { |options|
      if options.validate
        RelationGraphValidationTool.check_options(options)
      end
    }
  end

  # pml ... PML for the prgoam
  # XXX: this is just a stub, there is more work to do
  def RelationGraphTransformTool.run(pml,options)
    if(options.validate)
      RelationGraphValidation.new.validate
    end

    # Builder and Analysis Entry
    ilp  = ILP.new
    builder_opts = options.dup
    builder_opts.use_relation_graph = true
    ipet = IPETBuilder.new(pml,builder_opts,ilp)
    entry = pml.machine_functions.by_label(options.analysis_entry)

    # flow facts
    ff_types = options.flow_fact_types
    ff_types = RelationGraphTransformTool::SUPPORTED_FLOW_FACT_TYPES if ff_types == :supported
    flowfacts = pml.flowfacts.filter(ff_types, options.flow_fact_srcs,  ["bitcode","machinecode"])
    # Refine Control-Flow Model
    ipet.refine(entry, flowfacts)

    # Build IPET, no costs
    ipet.build(entry) { |edge| 0 }

    # Add flow facts
    flowfacts.each { |ff|
      name = ipet.add_flowfact(ff)
      # puts "#{name}: #{show_constraint(ff.lhs.map { |t| [t.ppref.qname, t.factor] },ff.op,ff.rhs)}"
    }
    # If direction up/down, eliminate all vars but dst/src
    ### XXX: up-down transform
    target_level = :src
    ilp.variables.each do |var|
      if ilp.vartype[var] != target_level
        ilp.eliminate(var)
      end
    end
    ilp.constraints.each do |name,lhsi,op,rhs|
      lhs = Hash.new(0)
      lhsi.each { |vi,c| lhs[ilp.variables[vi-1]] = c }
      next if name =~ /^__lower_bound/ && rhs == 0
      next if name =~ /^structural/
      next if name =~ /^rg/
      next if name =~ /^callsite/
      next if name =~ /^calledges/

      puts "#{name} (pre) #{show_constraint(lhs.to_a,op,rhs)}"

      # make flow-fact

      # Simplify: edges->block
      # (1) get all referenced outgoing blocks
      out_blocks = {}
      lhs.each { |edge,coeff| out_blocks[edge.source] = 0 }
      # (2) for each block, find minimum coeff for all of its outgoing edges
      #     and replace edges by block
      out_blocks.keys.each { |b|
        edges = b.successors.map { |b2| Edge.new(b,b2,target_level) }
        edges = [ Edge.new(b,:exit, target_level) ] if edges.empty?
        min_coeff = edges.map { |e| lhs[e] }.min
        if min_coeff != 0
          edges.each { |e| lhs[e] -= min_coeff ; lhs.delete(e) if lhs[e] == 0 }
          lhs[b] += min_coeff
        end
      }
      # (1) detect scope
      # (3) simplify: scope
      # (1) if main/entry or main/exit appears with frequency 1, this is a global
      # constraint
      puts "#{name} #{show_constraint(lhs.to_a,op,rhs)}"
    end
    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF
Validates relation graph and transforms flow facts from IR level to machine code level.
EOF
  options, args = PML::optparse([:input], "file.pml", SYNOPSIS) do |opts|
    RelationGraphTransformTool.add_options(opts)
  end
  RelationGraphTransformTool.run(PMLDoc.from_file(options.input), options)
end
