#
# platin tool set
#
# support for context sensitivity
#
require 'core/pml'

module PML

# Stacks with bounded memory
class BoundedStack
  # fly-weight, to get cheaper comparisons (profile hotspot)
  @@repository = {}
  attr_reader :stack
  def BoundedStack.empty
    BoundedStack.create([])
  end
  def BoundedStack.create(stack)
    bs = @@repository[stack]
    if ! bs
      bs = @@repository[stack] = BoundedStack.new(stack)
    end
    bs
  end
  def initialize(elems)
    @stack = elems
    @cache = {}
  end
  def empty?
    @stack.empty?
  end
  def length
    @stack.length
  end
  def push(elem, maxlength)
    return if maxlength == 0
    return BoundedStack.create([elem]) if maxlength==1
    if stack.length < maxlength
      BoundedStack.create(stack + [elem])
    else
      BoundedStack.create(stack[1..-1] + [elem])
    end
  end
  def prefix(length)
    return BoundedStack.create([]) unless length > 0
    BoundedStack.create(stack.dup[0..(length-1)])
  end
  def pop
    newstack = stack.dup
    newstack.pop
    BoundedStack.create(newstack)
  end
  def top
    @stack[-1]
  end
  def map_top
    newstack = stack.dup
    newelem = yield newstack.pop
    newstack.push(newelem)
    BoundedStack.create(newstack)
  end
  # return array of stack elements, top-most first
  def to_a
    @stack.reverse
  end
  def to_s
    return "[]" if @stack.empty?
    @stack.map { |n| n.to_s }.join(",")
  end
  # unique objects - can use pointer equality (== defined as equals?) per default
  def hash
    return @hash if @hash
    @hash = stack.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

#
# Call-Context Tree
#
# Store call-context-sensitive information (e.g., infeasibility), in a
# tree
#
# Context node for storing context-sensitive information
class ContextTree
  attr_reader :root
  def initialize
    @root = ContextNode.new
  end
  # Add a value in the context, given as (reversed) context list
  # For example, the context list for the callstack suffix
  # f/1 -> g/2 should be [g/2, f/1].
  # Example: <tt>@infeasible.set(callstring.to_a, true)@</tt>
  def set(ctx_list, value)
    node = @root
    ctx_list.each do |ctx_item|
      node = node.add_child(ctx_item)
    end
    node.value = value
  end
  # find first node on path with defined value
  def path_find_first(path)
    node = @root
    return node.value unless node.value.nil?
    path.each { |path_item|
      node = node.get_child(path_item)
      return nil        unless node
      return node.value unless node.value.nil?
    }
    nil
  end
  # find last node on path with defined value
  def path_find_last(path)
    node = @root
    value = @root.value
    path.each { |path_item|
      node = node.get_child(path_item)
      return value       unless node
      value = node.value unless node.value.nil?
    }
    value
  end
  def empty?
    @root.empty?
  end
  def dump(io=$stdout,level = 0)
    @root.dump(io,level)
  end
end

class ContextNode
  attr_accessor :value
  def initialize
    @value, @children = nil, {}
  end
  def empty?
    @value.nil? && @children.empty?
  end
  def get_child(ctx_item)
    @children[ctx_item]
  end
  def add_child(ctx_item)
    if c = get_child(ctx_item)
      c
    else
      @children[ctx_item] = ContextNode.new
    end
  end
  def dump(io=$stdout, level = 0)
    io.puts " "*(level*2) + "NODE VALUE #{@value.to_s}" if @value
    @children.each { |key,node|
      io.puts " "*(level*2 + 1) + key.to_s
      node.dump(io, level+1)
    }
  end
end

#
# callstring entry (either call edge or loop edge)
#
class ContextEntry < PMLObject
  def ContextEntry.from_pml(functions,data)
    if data['loop']
      LoopContextEntry.from_pml(functions,data)
    else
      CallContextEntry.from_pml(functions,data)
    end
  end
end

class CallContextEntry < ContextEntry
  attr_reader :callsite,:callee
  def initialize(callsite, data = nil)
    @callsite = callsite
    set_yaml_repr(data)
  end
  def to_pml
    { 'callsite' => @callsite.qname }
  end
  def CallContextEntry.from_pml(functions, data)
    ref = Instruction.from_qname(functions,data['callsite'])
    assert("CallContextEntry#from_pml: callsite is not an instruction") { ref.kind_of?(Instruction) }
    CallContextEntry.new(ref.instruction, data)
  end
  def dup
    CallContextEntry.new(callsite)
  end
  def inspect
    "#<CallContextEntry #{object_id}: #{callsite}>"
  end
  def to_s
    @callsite.qname
  end
  def qname
    @callsite.qname
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(CallContextEntry)
    @callsite == other.callsite
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash = callsite.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

#
# Context Entry (Loop or Call)
# Loop Context
#  callsite    ... the program point that branched to the loop entry, or TOP (used by aiT)
#  offset/step ... index of loop iterations is { offset + step*k | k >= 0 }
#  TOP element ... (callsite=TOP,offset=0,step=1)
# Call Context
#  callsite    ... the instruction that called the function entry, or TOP
#  TOP element ... (callsite=TOP)
#
class LoopContextEntry < ContextEntry
  attr_reader :callsite,:loop,:offset,:step

  def initialize(loop,step=1,offset=0,callsite=nil,data=nil)
    assert("Loop context: no loop given (#{loop})") { loop.kind_of?(Block) }
    @loop, @step, @offset, @callsite = loop,step,offset,callsite
    @callsite = nil if @loop.has_preheader? # no interesting information
    set_yaml_repr(data)
  end

  # XXX: little bit hackish
  def to_pml
    @data = { 'loop' => @loop.qname,
              'step' => @step,
              'offset' => @offset }
    @data['callsite'] = @callsite.qname if @callsite
    @data
  end

  def LoopContextEntry.from_pml(functions, data)
    ref = Loop.from_qname(functions,data['loop'])
    assert("LoopContextEntry#from_pml: loop is not a loop reference") { ref.kind_of?(Loop) }
    step = data['step']
    offset = data['offset']
    callsite = Instruction.from_qname(functions, data['callsite']).instruction if data['callsite']
    LoopContextEntry.new(ref.loopheader,step,offset,callsite)
  end

  #
  # check whether the loop context matches the peel/unroll
  # setting and normalize
  #
  # Check:
  #  (step = 0) v (step = unroll ^ offset >= peel)
  # Normalized
  #  (step = 0 ^ offset < peel) v (step = unroll ^ offset = peel + k ^ k < unroll)
  def check_and_normalize(offset,step,peel,unroll)
    raise Exception.new("LoopContextEntry: step #{step} or offset #{offset} negative") if offset < 0 || step < 0
    if step != 0
      assert("LoopContextEntry: step #{step} does not match unroll factor #{unroll}") { step == unroll }
      assert("LoopContextEntry: offset #{offset} to small for peel factor #{peel}")   { offset >= peel }
      [peel + (offset - peel)%unroll, unroll]
    else
      if offset >= peel
        [peel + (offset-peel)%unroll, unroll]
      else
        [offset, 0]
      end
    end
  end

  #
  # Given that we are in loop iteration (offset+step*k),
  # compute next iterations (offset+step*k + 1)
  # Assumes the expression is normalized
  def with_increment(peel,unroll)
    o,s = check_and_normalize(@offset+1,@step,peel,unroll)
    LoopContextEntry.new(@loop,s,o,@callsite)
  end

  #
  # Given that we are in loop iteration (offset+step*k),
  # compute previous iterations (offset-1+step*k), a set of expressions
  def predecessor_contexts(peel, unroll)
    # handle the case when subtracting one from offset violates normalization
    o,s = check_and_normalize(@offset, @step, peel, unroll)
    if o == 0 # implies @step == 0
      []
    elsif o == peel
      [ LoopContextEntry.new(@loop,0,peel-1,@callsite),
        LoopContextEntry.new(@loop,unroll,peel+unroll-1,@callsite) ]
    else
      o2, s2 = check_and_normalize(@offset - 1, @step, peel, unroll)
      [ LoopContextEntry.new(@loop, s2, o2, @callsite) ]
    end
  end

  def dup
    LoopContextEntry.new(@loop,@step,@offset,@callsite)
  end
  def to_s
    qname
  end
  def inspect
    "#<LoopContextEntry #{object_id}: #{callsite}->#{loop} #{offset}+#{step} k>"
  end
  def qname
    return @qname if @qname
    lc = @offset.to_s
    lc += "+k" if @step == 1
    lc += "+#{@step}k" if @step > 1
    @qname = "#{loop.qname}[#{lc}]#{@callsite ? @callsite.qname : ''}"
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(LoopContextEntry)
    if ! other.callsite.nil?
      return false unless callsite && callsite == other.callsite
    else
      return false unless callsite.nil?
    end
    @loop == other.loop && @step == other.step && @offset == other.offset
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash = loop.hash ^ offset.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

# Context
class Context < PMLObject
  attr_reader :callstring
  def initialize(callstring, data=nil)
    assert("Context: expecting BoundedStack") {
      callstring.kind_of?(BoundedStack)
    }
    @callstring = callstring
    set_yaml_repr(data)
  end
  def to_pml
    @callstring.to_a.map { |cse| cse.data }
  end
  def Context.from_pml(functions, data)
    cs = data.map { |ref| ContextEntry.from_pml(functions,ref) }
    Context.new(BoundedStack.create(cs.reverse), data)
  end
  def qname
    return @callstring.to_a.map { |cs| "(#{cs.qname})" }.join("")
  end
  def Context.empty
    Context.new(BoundedStack.empty)
  end
  def empty?
    @callstring.empty?
  end
  def push(e, bound)
    Context.new(@callstring.push(e,bound))
  end
  def pop
    Context.new(@callstring.pop)
  end
  def top
    @callstring.top
  end
  def map_top
    Context.new(@callstring.map_top { |e| yield e })
  end
  def Context.callstack_suffix(stack, length)
    return Context.new(BoundedStack.empty) if length == 0
    start = (length >= stack.length) ? 0 : (-length)
    entries = stack[start..-1].map { |callsite|
      CallContextEntry.new(callsite)
    }
    Context.new(BoundedStack.create(entries))
  end
  def Context.from_list(list)
    assert("Context.from_list: not a list of context entries (#{list.first.class})") {
      list.all? { |e| e.kind_of?(ContextEntry) }
    }
    Context.new(BoundedStack.create(list.reverse))
  end
  def to_a
    @callstring.to_a
  end
  def scope_context
    # no dynamic context at the moment
    self
  end
  def with_scope_context(scope_context)
    # no non-scoped context at the moment
    scope_context
  end
  def to_s
    list = @callstring.to_a.map { |entry| entry }.join(",")
    "Context<#{list}>"
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(Context)
    @callstring == other.callstring
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash = @callstring.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

#
# program point + context
class ContextRef < PMLObject
  attr_reader :programpoint, :context
  def initialize(pp, context, data = nil)
    assert("ContextRef: programpoint has bad type #{pp.class}") { pp.kind_of?(ProgramPoint) }
    assert("ContextRef: context has bad type #{context.class}") { context.kind_of?(Context) }
    @programpoint, @context = pp, context
    set_yaml_repr(data)
  end

  #
  # FIXME: the notation is compact, but merges YAML representation of programpoint references and context
  # Is this a good idea?
  def to_pml
    pml = @programpoint.to_pml_ref
    pml['context'] = @context.data unless @context.empty?
    pml
  end

  def ContextRef.from_pml(functions, data)
    context = data['context'] ? Context.from_pml(functions,data['context']) : Context.empty
    pp = ProgramPoint.from_pml(functions, data)
    ContextRef.new(pp, context, data)
  end
  def function
    programpoint.function
  end
  def block
    programpoint.block
  end
  def instruction
    programpoint.instruction
  end
  def loop
    programpoint.loop
  end
  def qname
    "#{programpoint.qname}#{context.qname}"
  end
  def inspect
    "ContextRef<programpoint=#{programpoint.inspect},context=#{context.inspect}>"
  end
  def to_s
    "#{programpoint}#{context.qname}"
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(ContextRef)
    @programpoint == other.programpoint && @context == other.context
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash = @programpoint.hash * 31 + @context.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end


#
# fairly generic context manager
# loop contexts are disabled if looppeel == 0 and loopunroll == 1
class ContextManager

  def ContextManager.create(history_length, looppeel = 0, loopunroll = 1)
    return ContextManagerEmpty.new if history_length < 1
    return ContextManager.new(history_length, looppeel, loopunroll)
  end

  def initialize(history_length, looppeel = 0, loopunroll = 1)
    @history_length, @looppeel, @loopunroll = history_length, looppeel, loopunroll
  end
  def initial
    Context.empty
  end
  # do not record instruction history context for now
  def blockslice(ctx, node)
    ctx
  end
  def store_loopcontext?
    @looppeel != 0 || @loopunroll != 1
  end
  def push_call(ctx, callsite)
    ctx.push(CallContextEntry.new(callsite), @history_length)
  end
  def pop_call(ctx)
    ctx_entry = ctx.top
    if ctx_entry
      assert("pop_call: need to exit loop first") { ctx_entry.kind_of?(CallContextEntry) }
      ctx_entry = ctx_entry.callsite
    end
    [ ctx.pop, ctx_entry ]
  end
  def enter_loop(ctx, loop)
    return ctx unless store_loopcontext?
    ctx.push(LoopContextEntry.new(loop), @history_length)
  end
  def exit_loop(ctx)
    return ctx unless store_loopcontext?
    assert("exit_loop: not a loop context #{ctx.top}") { ctx.top.kind_of?(LoopContextEntry) }
    loopnode = ctx.top.loop
    [ ctx.pop, loopnode ]
  end
  def continue_loop(ctx)
    return ctx unless store_loopcontext?
    ctx.map_top { |lc|
      assert("continue_loop: not a loop context #{lc}") { lc.kind_of?(LoopContextEntry) }
      lc.with_increment(@looppeel, @loopunroll)
    }
  end
  def reset_loop(ctx, loop)
    return ctx unless store_loopcontext?
    ctx.map_top { |lc|
      assert("continue_loop: not a loop context #{lc}") { lc.kind_of?(LoopContextEntry) }
      LoopContextEntry.new(loop)
    }
  end
end

#
# Trivial context manager (no history)
#
class ContextManagerEmpty
  def initialize ; end
  def initial ; Context.empty ; end
  def blockslice(ctx, node) ; ctx ; end
  def store_loopcontext? ; false ; end
  def push_call(ctx,_); ctx ; end
  def pop_call(ctx,_) ; ctx ; end
  def exit_loop(ctx) ; ctx; end
  def continue_loop(ctx); ctx; end
  def reset_loop(ctx); ctx; end
end

end # module PML

# in-module testing
if __FILE__ == $0
  require 'test/unit'
  include PML
  class TestContext < Test::Unit::TestCase
    def setup
      @ctxm1 = ContextManager.new(2)
      @ctxm2 = ContextManager.new(2,1,2)
    end
    def sd_instructions(num)
      (0..num-1).map { |ix| { 'index' => ix } }
    end
    def sd_function(name)
      b0 = { 'name' => 0, 'instructions' => sd_instructions(6) }
      { 'name' => name, 'blocks' => [b0] }
    end
    def sd_flist
      [sd_function('main'),sd_function('f'),sd_function('g'),sd_function('h')]
    end
    def test_call_return1
      c = @ctxm1.initial
      fs = FunctionList.new(sd_flist, :labelkey => 'name')
      c1 = fs['main'].blocks.first.instructions.first
      c2 = fs['f'].blocks.first.instructions.first
      c = @ctxm1.push_call(c, c1)
      c = @ctxm1.push_call(c, c2)
      c_from = Context.from_pml(fs,c.data)
      assert_equal(c, c_from)
      c, site = @ctxm1.pop_call(c)
      assert_equal(c2,site)
      c, site = @ctxm1.pop_call(c)
      assert_equal(c1,site)
      assert_equal(@ctxm1.initial, c)
    end
    def test_call_return2
      c = @ctxm1.initial
      fs = FunctionList.new(sd_flist, :labelkey => 'name')
      c1 = fs['main'].blocks.first.instructions.first
      c2 = fs['f'].blocks.first.instructions.first
      c3 = fs['g'].blocks.first.instructions.first
      l1 = fs['f'].blocks.first
      c = @ctxm1.push_call(c, c1)
      c = @ctxm1.push_call(c, c2)
      c = @ctxm1.enter_loop(c, l1)
      c = @ctxm1.push_call(c, c3)
      c, site = @ctxm1.pop_call(c)
      assert_equal(c3,site)
      c = @ctxm1.continue_loop(c)
      c = @ctxm1.push_call(c, c3)
      c, site = @ctxm1.pop_call(c)
      c, loop = @ctxm1.exit_loop(c)
      c, site = @ctxm1.pop_call(c)
      assert_equal(c2,site)
      c, site = @ctxm1.pop_call(c)
      assert_equal(@ctxm1.initial, c)
    end
    def test_loop
      c = @ctxm2.initial
      fs = FunctionList.new(sd_flist, :labelkey => 'name')
      c1 = fs['main'].blocks.first.instructions.first
      c2 = fs['f'].blocks.first.instructions.first
      c3 = fs['g'].blocks.first.instructions.first
      l1 = fs['f'].blocks.first
      c = @ctxm2.push_call(c, c1)
      c = @ctxm2.enter_loop(c, l1)
      c_from = Context.from_pml(fs,c.data)
      assert_equal(c, c_from)
      c = @ctxm2.push_call(c, c2)
      c, site = @ctxm2.pop_call(c)

      assert_equal(0,c.top.offset)
      assert_equal(1,c.top.step)
      c = @ctxm2.continue_loop(c)
      assert_equal(1,c.top.offset)
      assert_equal(2,c.top.step)
      c = @ctxm2.continue_loop(c)
      assert_equal(2,c.top.offset)
      assert_equal(2,c.top.step)
      c = @ctxm2.push_call(c, c2)
      c, site = @ctxm2.pop_call(c)
      c, loop = @ctxm2.exit_loop(c)
      assert_equal(loop, l1)
      c, site = @ctxm2.pop_call(c)
      assert_equal(@ctxm2.initial, c)
      c, site = @ctxm2.pop_call(c)
      assert_equal(@ctxm2.initial, c)
    end
  end
end
