#
# PSK Toolchain
#
# Relation Graph utilities: validate, flow fact transformation
#

require 'utils'
require 'sweet'
require 'trace'
include PML

class ProgressTraceRecorder
  attr_reader :level, :trace
  def initialize(pml, entry, is_machine_code)
    @pml = pml
    @options = OpenStruct.new
    @trace, @entry, @level = [], entry, is_machine_code ? :dst : :src
    @callstack = []
  end
  # set current relation graph
  # if there is no relation graph, skip function
  def function(callee,callsite,cycles)
    @rg = @pml.relation_graphs.by_name(callee.name, @level)
    puts "Found rg for #{@level}-#{callee}: #{@rg.nodes.first}" if @rg && @options.debug
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
      puts "Visiting node #{@node}" if @options.debug
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
      trace.push(succ) if(succ.type == :progress)
      @node = succ
      $dbgs.puts "Visiting node: #{@node}" if @options.debug
    end
  end
  # set current relation graph
  def ret(rsite,csite,cycles)
    return if csite.nil?
    @rg = @pml.relation_graphs.by_name(csite.function.name, @level)
    @node = @callstack.pop
    #puts "Found rg for #{@level}-#{csite.function}: #{@rg.nodes.first}" if @rg
  end
  def eof ; end
  def method_missing(event, *args); end
end

class RelationGraphValidation
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
      raise Exception.new("Progress trace validation failed: #{p1} != #{p2}") if p1 != p2
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
  def RelationGraphValidationTool.add_options(opts)
    opts.analysis_entry
    opts.binary_file(true)
    opts.pasim
    opts.sweet_trace_file
  end
  def RelationGraphValidationTool.run(pml, options)
    mtrace = SimulatorTrace.new(options.binary_file, options.pasim)
    tm1 = MachineTraceMonitor.new(pml, mtrace)
    entry  = pml.machine_functions.by_label(options.analysis_entry)
    pt1 = ProgressTraceRecorder.new(pml, entry, true)
    tm1.subscribe(pt1)
    tm1.run

    tm2 = SWEET::TraceMonitor.new(options.sweet_trace_file, pml)
    pt2 = ProgressTraceRecorder.new(pml, options.analysis_entry, false)
    tm2.subscribe(pt2)
    tm2.run
    RelationGraphValidation.new(pml).validate(pt2, pt1)
  end
end

class RelationGraphTransformTool
  def RelationGraphTransformTool.add_options(opts)
    opts.analysis_entry
    opts.flow_fact_selection
    opts.generates_flowfacts
    opts.on("--validate", "Validate relation graph") { opts.options.validate = true }
    RelationGraphValidationTool.add_options(opts)
    opts.add_check { |options|
      if options.validate
        die_usage("Binary file is needed for validation. Try --help") unless options.binary_file
      end
    }
  end

  # pml ... PML for the prgoam
  # XXX: this is just a stub, there is more work to do
  def RelationGraphTransformTool.run(pml,options)
    if(options.validate)
      RelationGraphValidation.new.validate
    end
    ilp  = ILP.new
    ipet = IPETBuilder.new(ilp)
    ipet.build(pml,options)
    level = nil
    pml.flowfacts.each { |ff|
      if ff.matches(options.flow_fact_types, options.flow_fact_srcs)
        if level && ff.level != level
          die("Flow Fact Transform: selected flow facts from both bitcode and machinecode")
        end
        level = ff.level
        ipet.add_flowfact(ff)
      end
    }
    ilp.variables.each do |var|
      if ilp.vartype[var] != :dst
        ilp.eliminate(var)
      end
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
  RelationGraphValidationTool.run(PMLDoc.from_file(options.input), options)
end
