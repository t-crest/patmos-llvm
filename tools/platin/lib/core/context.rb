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
  def BoundedStack.initial(length)
    BoundedStack.create([])
  end
  def BoundedStack.suffix(stack, length)
    return BoundedStack.create(stack.dup) if length > stack.length
    return BoundedStack.initial(0) if length == 0
    BoundedStack.create(stack[-length..-1])
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
    @stack.map { |n| n.to_s }.join("-")
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

# Loop Context for one loop
# Given peel and unroll factors
# loop contexts represented as
#   [loopheader,init,offset]
# correspond to the loop iteration
#   init<peel:  init
#   init==peel: peel + unroll*k + offset
class LoopContext
  attr_reader :loop,:init,:offset
  def initialize(loop,init=0,offset=0)
    raise Exception.new("Loop Context: loop.nil?") unless loop
    @loop = loop
    @init = init
    @offset = offset
  end
  def dup
    LoopContext.new(@loop,@init,@offset)
  end
  def with_increment(peel,unroll)
    if @init >= peel
      LoopContext.new(@loop, @init,(@offset + 1) % unroll)
    else
      LoopContext.new(@loop, @init+1,@offset)
    end
  end
  def predecessor_contexts(peel, unroll)
    if @init >= @peel
      if @offset == 0
        [ LoopContext.new(@loop,peel-1,0), LoopContext.new(@loop, @init, unroll - 1) ]
      else
        [ LoopContext.new(@loop,@init,@offset-1) ]
      end
    elsif @init > 0
      [ LoopContext.new(@loop, @init - 1, 0) ]
    else
      []
    end
  end
  def to_s
    "#<LoopContext #{loop} #{init} #{offset}>"
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(LoopContext)
    @loop == other.loop && @init == other.init && @offset == other.offset
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash = @loops.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

# Context
class Context
  attr_reader :callstring, :loopcontext
  def initialize(cs, lc)
    @callstring, @loopcontext = cs, lc
  end
  def Context.initial(calldepth, loopdepth)
    Context.new(BoundedStack.initial(calldepth), BoundedStack.initial(loopdepth))
  end
  def to_a
    @callstring.to_a + @loopcontext.to_a
    # @callstring.reverse.each { |s| yield [:callsite,s] }
    # @loopcontext.stack.reverse.each { |s| yield [:loopcontext,s] }
  end
  def with_callstring
    callstring_new = yield @callstring
    Context.new(callstring_new, @loopcontext)
  end
  def with_loopcontext
    loopcontext_new = yield @loopcontext
    Context.new(@callstring, loopcontext_new)
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
    "Ctx<#{@callstring},#{@loopcontext}>"
  end
  def ==(other)
    return false if other.nil?
    return false unless other.kind_of?(Context)
    @callstring == other.callstring && @loopcontext == other.loopcontext
  end
  def eql?(other); self == other ; end
  def hash
    return @hash if @hash
    @hash = @callstring.hash ^ @loopcontext.hash
  end
  def <=>(other)
    hash <=> other.hash
  end
end

# fairly generic context manager
class ContextManager
  def initialize(calldepth = 1, loopdepth = 0, looppeel = 1, loopunroll = 1)
    raise Exception.new("ContextManager: calldepth>=1 is required") unless calldepth >= 1
    @calldepth, @loopdepth, @looppeel, @loopunroll = calldepth, loopdepth, looppeel, loopunroll
  end
  def initial
    Context.initial(@calldepth, @loopdepth)
  end
  # do not record instruction history context for now
  def blockslice(ctx, node)
    ctx
  end
  def push_call(ctx, callnode)
    ctx.with_callstring { |cs| cs.push(callnode, @calldepth) }
  end
  def pop_call(ctx)
    callnode = ctx.callstring.top
    if callnode
      [ ctx.with_callstring { |cs| cs.pop }, callnode ]
    else
      nil
    end
  end
  def enter_loop(ctx, loop)
    return ctx unless @loopdepth > 0
    ctx.with_loopcontext { |lc|
      lc.push(LoopContext.new(loop), @loopdepth)
    }
  end
  def exit_loop(ctx)
    return ctx unless @loopdepth > 0
    loopnode = ctx.loopcontext.top.loop
    [ ctx.with_loopcontext { |lc| lc.pop }, loopnode ]
  end
  def continue_loop(ctx)
    return ctx unless @loopdepth > 0
    ctx.with_loopcontext { |lcs|
      lcs.map_top { |lc| lc.with_increment(@looppeel, @loopunroll) }
    }
  end
  def reset_loop(ctx, loop)
    return ctx unless @loopdepth > 0
    ctx.with_loopcontext { |lcs|
      lcs.map_top { |lc| LoopContext.new(loop) }
    }
  end
end

end # module PML
