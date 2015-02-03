#
# The *platin* toolkit
#
# FFX import and export, F4 export
#

require 'platin'
require "rexml/document"
require "rexml/formatters/transitive"

# OTAWA currently does not know about all labels, so export address references
# instead of labels right now
EXPORT_ADDR = true

module PML

# option extensions for f4/ffx
class OptionParser
  def ff_file(mandatory=true)
    self.on("--ff FILE", "Path to F4/FFX file") { |f| options.ff_file = f }
    self.add_check { |options| die_usage "Option --ff is mandatory" unless options.ff_file } if mandatory
  end
end


# Features not supported by the FFX export module
class F4UnsupportedFeatureException < Exception
  def initialize(msg)
    super(msg)
  end
end

# Program Points not supported by the FFX export module
class F4UnsupportedProgramPoint < F4UnsupportedFeatureException
  def initialize(pp, msg = "information references program point not supported by F4 exporter")
    super("#{msg} (#{pp} :: #{pp.class})")
    @pp = pp
  end
end

# Features not supported by the FFX export module
class FFXUnsupportedFeatureException < Exception
  def initialize(msg)
    super(msg)
  end
end

# Program Points not supported by the FFX export module
class FFXUnsupportedProgramPoint < FFXUnsupportedFeatureException
  def initialize(pp, msg = "information references program point not supported by FFX exporter")
    super("#{msg} (#{pp} :: #{pp.class})")
    @pp = pp
  end
end


#
# Extend ValueRange with +#to_ffx+
#
class ValueRange
  # Either the symbol this range references (double quoted), or a numeric range
  def to_f4
    if s = self.symbol
      dquote(s)
    else
      raise Exception.new("#{self.class}#to_f4: no translation available")
    end
  end

  def to_ffx
    raise Exception.new("#{self.class}#to_ffx: no translation available")
  end

  def ValueRange.range_to_ffx(range)
    sprintf("0x%08x .. 0x%08x",range.min,range.max)
  end
end

#
# Extend SymbolicExpression (and sublcassoes) with +#to_ffx+
#
class SymbolicExpression
  def to_f4
    raise Exception.new("#{self.class}#to_f4: no translation available")
  end
  def to_ffx
    raise Exception.new("#{self.class}#to_ffx: no translation available")
  end
end

class SEInt
  def to_f4  ; self.to_s ; end
  def to_ffx ; self.to_s ; end
end

# Variables always reference arguments of functions
class SEVar
  def to_f4  ; "@arg_#{self}" ; end
  def to_ffx ; "@arg_#{self}" ; end
end

class SEBinary
  def to_f4
    raise Exception.new("#{self.class}#to_f4: no translation available")
  end
  def to_ffx
    raise Exception.new("#{self.class}#to_ffx: no translation available")
#    left,right = self.a, self.b
#    if SEBinary.commutative?(op) && left.constant? && ! right.constant?
#      left, right = right, left
#    end
#    lexpr, rexpr = [left,right].map { |v| v.to_ffx }
#    # simplify (x * -1) to (-x)
#    if right.constant? && right.constant == -1 && op == '*'
#      return "-(#{lexpr})"
#    end
#    # simplify (a + -b) to (a - b)
#    if right.constant? && right.constant < 0 && op == '+'
#      return "#{lexpr} - #{-right}"
#    end
#    # translation of all other ops
#    case op
#    when '+'    then "(#{lexpr} + #{rexpr})"
#    when '*'    then "(#{lexpr} * #{rexpr})"
#    when '/u'   then "(#{lexpr} / #{rexpr})"
#    when 'umax' then "max(#{lexpr},#{rexpr})"
#    when 'umin' then "min(#{lexpr},#{rexpr})"
#      # FIXME: how do we deal with signed variables?
#    when 'smax' then "max(#{lexpr},#{rexpr})"
#    when 'smin' then "min(#{lexpr},#{rexpr})"
#    when '/s'   then "(#{lexpr} / #{rexpr})"
#    else        raise Exception.new("SymbolicExpression#eval: unknown binary operator #{@op}")
#    end
  end
end


#
# Extend program points with +#ffx_ref+
#

class Function
  def f4_ref
    if self.label && !EXPORT_ADDR
      dquote(self.label)
    elsif self.address
      "0x#{address.to_s(16)}"
    else
      raise F4UnsupportedProgramPoint.new(self, "neither address nor label available (forgot 'platin extract-symbols'?)")
    end
  end
  def ffx_ref
    raise Exception.new("#{self.class}#to_ffx: no translation available")
  end
end

class Block
  def f4_ref
    if instructions.empty?
      raise F4UnsupportedProgramPoint.new(self, "impossible to reference an empty block")
    end
    if label && !EXPORT_ADDR
      dquote(label)
    elsif address
      "0x#{address.to_s(16)}"
    else
      raise F4UnsupportedProgramPoint.new(self, "neither address nor label available (forgot 'platin extract-symbols'?)")
    end
  end
  def ffx_ref
    raise Exception.new("#{self.class}#to_ffx: no translation available")
  end
end

class Instruction
  def f4_ref(opts = {})
    if address && block.label && !EXPORT_ADDR
      "#{block.f4_ref} + #{self.address - block.address}"
    elsif address
      "0x#{address.to_s(16)}"
    else
      raise F4UnsupportedProgramPoint.new(self, "neither address nor symbolic offset available (forgot 'platin extract-symbols'?)")
    end
  end
end

class Loop
  # no automatic translation for loops
  def f4_ref
    raise F4UnsupportedProgramPoint.new(self)
  end
  def ffx_ref
    raise FFXUnsupportedProgramPoint.new(self)
  end
end

class Edge
  def f4_ref
    raise F4UnsupportedProgramPoint.new(self)
  end
  def ffx_ref
    raise FFXUnsupportedProgramPoint.new(self)
  end
end


# class to export PML information to FFX
class F4Exporter

  attr_reader :stats_generated_facts,  :stats_skipped_flowfacts
  attr_reader :outfile, :options

  def initialize(pml, f4_file, options)
    @pml = pml
    @outfile = f4_file
    @options = options
    @entry = @pml.machine_functions.by_label(@options.analysis_entry)
    @stats_generated_facts, @stats_skipped_flowfacts = 0, 0
  end

  def gen_fact(f4_fact, descr, derived_from=nil)
    @stats_generated_facts += 1
    @outfile.puts(f4_fact+";" +" // "+descr)
    debug(@options,:ffx) {
      s = " derived from #{derived_from}" if derived_from
      "Wrote F4 instruction: #{f4_fact}#{s}"
    }
    true
  end

  def merge_file(file)
    @outfile.puts(File.read(file))
  end

  # Export jumptables for a function
  def export_jumptables(func)
    func.blocks.each do |mbb|
      branches = 0
      mbb.instructions.each do |ins|
        branches += 1 if ins.branch_type && ins.branch_type != "none"
        if ins.branch_type == 'indirect'
          successors = ins.branch_targets ? ins.branch_targets : mbb.successors
          targets = successors.uniq.map { |succ|
            succ.f4_ref
          }.join(", ")
	  # TODO where does the flow fact actually come from?
          gen_fact("multibranch #{ins.f4_ref(:branch_index => branches)} to #{targets}","jumptable (source: llvm)",ins)
        end
      end
    end
  end

  # export indirect calls
  def export_calltargets(ff, scope, callsite, targets)
    assert("Bad calltarget flowfact: #{ff.inspect}") { scope && scope.context.empty? }

    # no support for context-sensitive call targets
    unless callsite.context.empty?
      warn("F4: no support for callcontext-sensitive callsites")
      return false
    end

    called = targets.map { |f| f.f4_ref }.join(", ")
    gen_fact("multicall #{callsite.f4_ref} to #{called}",
             "global indirect call targets (source: #{ff.origin})",ff)
  end

  # export loop bounds
  def export_loopbounds(scope, bounds_and_ffs)

    # context-sensitive facts not yet supported
    unless scope.context.empty?
      warn("F4: callcontext-sensitive loop bounds not implemented")
      return false
    end
    loopblock = scope.programpoint.loopheader
    loopname = loopblock.f4_ref

    bounds_and_ffs.each { |bound,ff|
      if bound.referenced_vars.empty?
        gen_fact("loop #{loopname} #{bound.to_f4}",
                 "global loop header bound (source: #{ff.origin})")
      else
	warn("F4: symbolic loop bound #{bound} not supported")
        @stats_skipped_flowfacts += 1 
      end
    }
  end

  # export global infeasibles
  def export_infeasible(ff, scope, pp)
    # We let the analysis worry about that ..
  end

  def export_linear_constraint(ff)
    warn("F4: no support for linear constraints: #{ff}")
    return false
  end

  # export set of flow facts (minimum of loop bounds)
  def export_flowfacts(ffs)
    loop_bounds = {}

    ffs.each { |ff|
      if scope_bound = ff.get_loop_bound
        scope,bound = scope_bound
        next if options.ff_disable_export.include?('loop-bounds')
        next if ! bound.constant? && options.ff_disable_export.include?('symbolic-loop-bounds')
        (loop_bounds[scope]||=[]).push([bound,ff])
      else
        supported = export_flowfact(ff)
        @stats_skipped_flowfacts += 1 unless supported
      end
    }
    loop_bounds.each { |scope,bounds_and_ffs|
      export_loopbounds(scope, bounds_and_ffs)
    }
  end

  # export linear-constraint flow facts
  def export_flowfact(ff)
    assert("export_flowfact: loop bounds need to be exported separately") { ff.get_loop_bound.nil? }

    if (! ff.local?) && ff.scope.function != @entry
      warn("F4: non-local flow fact in scope #{ff.scope} not supported")
      false

    elsif ff.symbolic_bound?
      debug(options, :ffx) { "Symbolic Bounds only supported for loop bounds" }
      false

    elsif scope_cs_targets = ff.get_calltargets
      return false if options.ff_disable_export.include?('call-targets')
      export_calltargets(ff,*scope_cs_targets)

    elsif scope_pp = ff.get_block_infeasible
      return false if options.ff_disable_export.include?('infeasible-code')
      export_infeasible(ff,*scope_pp)

    elsif ff.blocks_constraint? || ff.scope.programpoint.kind_of?(Function)
      return false if options.ff_disable_export.include?('flow-constraints')
      export_linear_constraint(ff)

    else
      warn("F4: unsupported flow fact type: #{ff}")
      false
    end
  end

  # export value facts
  def export_valuefact(vf)
    assert("F4Exporter#export_valuefact: programpoint is not an instruction (#{vf.programpoint.class})") { vf.programpoint.kind_of?(Instruction) }
    if ! vf.ppref.context.empty?
      warn("F4Exporter#export_valuefact: cannot export context-sensitive program point")
      return false
    end
    rangelist = vf.values.map { |v| v.to_f4 }.join(", ")

    # TODO not yet supported
    #gen_fact("instruction #{vf.programpoint.ffx_ref}" + " accesses #{rangelist}",
    #         "Memory address (source: #{vf.origin})", vf)
  end

  # export stack cache instruction annotation
  def export_stack_cache_annotation(type, ins, value)
    assert("cannot annotate stack cache instruction w/o instruction addresses") { ins.address }
    if(type == :fill)
      feature = "stack_cache_fill_count"
    elsif(type == :spill)
      feature = "stack_cache_spill_count"
    else
      die("F4: unknown stack cache annotation")
    end

    # TODO not yet supported
    #gen_fact("instruction #{ins.ffx_ref} features \"#{feature}\" = #{value}", "SC blocks (source: llvm sca)")
  end
end


class FFXExporter

  attr_reader :stats_generated_facts,  :stats_skipped_flowfacts
  attr_reader :options

  def initialize(pml, options)
    @pml = pml
    @options = options
    @entry = @pml.machine_functions.by_label(@options.analysis_entry)
    @extracted_arguments = {}
    @stats_generated_facts, @stats_skipped_flowfacts = 0, 0

    doc = REXML::Document.new "<?xml version=\"1.0\"?><flowfacts></flowfacts>"
  end

  def merge_file(ffxfile)
    # TODO
  end

  def export_jumptables(func)
    func.blocks.each do |mbb|
      branches = 0
      mbb.instructions.each do |ins|
        branches += 1 if ins.branch_type && ins.branch_type != "none"
        if ins.branch_type == 'indirect'
          successors = ins.branch_targets ? ins.branch_targets : mbb.successors
          targets = successors.uniq.map { |succ|
            succ.f4_ref
          }.join(", ")
          #gen_fact("multibranch #{ins.f4_ref(:branch_index => branches)} to #{targets}","jumptable (source: llvm)",ins)
        end
      end
    end
  end

  # export indirect calls
  def export_calltargets(ff, scope, callsite, targets)
    assert("Bad calltarget flowfact: #{ff.inspect}") { scope && scope.context.empty? }

    # no support for context-sensitive call targets
    unless callsite.context.empty?
      warn("F4: no support for callcontext-sensitive callsites")
      return false
    end

    called = targets.map { |f| f.f4_ref }.join(", ")
    #gen_fact("multicall #{callsite.f4_ref} to #{called}",
    #         "global indirect call targets (source: #{ff.origin})",ff)
  end

  # export loop bounds
  def export_loopbounds(scope, bounds_and_ffs)

    # context-sensitive facts not yet supported
    unless scope.context.empty?
      warn("F4: callcontext-sensitive loop bounds not implemented")
      return false
    end
    loopblock = scope.programpoint.loopheader

    origins = Set.new
#    f4_bounds = bounds_and_ffs.map { |bound,ff|
#      # (1) collect registers needed (and safe)
#      # (2) generate symbolic expression
#      origins.add(ff.origin)
#      bound.referenced_vars.each { |v|
#        user_reg = @extracted_arguments[ [loopblock.function,v] ]
#        unless user_reg
#          user_reg = "@arg_#{v}"
#          @extracted_arguments[ [loopblock.function,v] ] = user_reg
#          gen_fact("instruction #{loopblock.function.ffx_ref} is entered with #{user_reg} = trace(reg #{v})",
#                   "extracted argument for symbolic loop bound")
#        end
#      }
#      bound.to_f4
#    }.uniq
#    bound = f4_bounds.length == 1 ? f4_bounds.first : "min(#{ffx_bounds.join(",")})"

    # As we export loop header bounds, we should say the loop header is 'at the end'
    # of the loop (confirmed by absint (Gernot))
#    loopname = dquote(loopblock.label)
#    gen_fact("loop #{loopname} max #{bound} end",
#             "global loop header bound (source: #{origins.to_a.join(", ")})")
  end

  # export global infeasibles
  def export_infeasible(ff, scope, pp)
    # We let the analysis worry about that ..
  end

  def export_linear_constraint(ff)
    warn("FFX: no support for linear constraints: #{ff}")
    return false
  end

  # export set of flow facts (minimum of loop bounds)
  def export_flowfacts(ffs)
    loop_bounds = {}
    ffs.each { |ff|
      if scope_bound = ff.get_loop_bound
        scope,bound = scope_bound
        next if options.ff_disable_export.include?('loop-bounds')
        next if ! bound.constant? && options.ff_disable_export.include?('symbolic-loop-bounds')
        (loop_bounds[scope]||=[]).push([bound,ff])
      else
        supported = export_flowfact(ff)
        @stats_skipped_flowfacts += 1 unless supported
      end
    }
    loop_bounds.each { |scope,bounds_and_ffs|
      export_loopbounds(scope, bounds_and_ffs)
    }
  end

  # export linear-constraint flow facts
  def export_flowfact(ff)
    assert("export_flowfact: loop bounds need to be exported separately") { ff.get_loop_bound.nil? }

    if (! ff.local?) && ff.scope.function != @entry
      warn("F4: non-local flow fact in scope #{ff.scope} not supported")
      false

    elsif ff.symbolic_bound?
      debug(options, :ffx) { "Symbolic Bounds only supported for loop bounds" }
      false

    elsif scope_cs_targets = ff.get_calltargets
      return false if options.ff_disable_export.include?('call-targets')
      export_calltargets(ff,*scope_cs_targets)

    elsif scope_pp = ff.get_block_infeasible
      return false if options.ff_disable_export.include?('infeasible-code')
      export_infeasible(ff,*scope_pp)

    elsif ff.blocks_constraint? || ff.scope.programpoint.kind_of?(Function)
      return false if options.ff_disable_export.include?('flow-constraints')
      export_linear_constraint(ff)

    else
      warn("aiT: unsupported flow fact type: #{ff}")
      false
    end
  end

  # export value facts
  def export_valuefact(vf)
    assert("F4Exporter#export_valuefact: programpoint is not an instruction (#{vf.programpoint.class})") { vf.programpoint.kind_of?(Instruction) }
    if ! vf.ppref.context.empty?
      warn("F4Exporter#export_valuefact: cannot export context-sensitive program point")
      return false
    end
    rangelist = vf.values.map { |v| v.to_f }.join(", ")

    # TODO not yet supported
    #gen_fact("instruction #{vf.programpoint.ffx_ref}" + " accesses #{rangelist}",
    #         "Memory address (source: #{vf.origin})", vf)
  end

  # export stack cache instruction annotation
  def export_stack_cache_annotation(type, ins, value)
    assert("cannot annotate stack cache instruction w/o instruction addresses") { ins.address }
    if(type == :fill)
      feature = "stack_cache_fill_count"
    elsif(type == :spill)
      feature = "stack_cache_spill_count"
    else
      die("F4: unknown stack cache annotation")
    end

    # TODO not yet supported
    #gen_fact("instruction #{ins.ffx_ref} features \"#{feature}\" = #{value}", "SC blocks (source: llvm sca)")
  end


  def write(outfile)
    # Mandatory to use transitive formatter
    REXML::Formatters::Transitive.new( 2, false ).write(@doc, outfile)
  end

  private
  def add_element(parent, name)
    el = REXML::Element.new(name, parent)
    yield el
    el
  end

  def rexml_bool(name, v)
    rexml_str(name, v ? 'true' : 'false')
  end

  def rexml_file(name, path)
    rexml_str(name, File.expand_path(path))
  end

  def rexml_str(name, v)
    el = REXML::Element.new(name)
    el << REXML::Text.new(v)
    el
  end
end

end # end module PML

