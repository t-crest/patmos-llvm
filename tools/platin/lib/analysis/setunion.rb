module SetUnion

class SetUnion
  attr_reader :map
  def initialize()
    @map = {}
  end

  class Node
    attr_reader :item
    attr_accessor :parent, :rank
    def initialize(item)
      @item, @parent, @rank = item, nil, 0
    end    
  end

  def to_set
    set_map = {}
    @map.each { |item,node|
      root = find_node(node)
      set_map[root] ||= Set.new
      set_map[root] << item
    }
    set = Set.new
    set_map.each { |root,union|
      set << union
    }
    set
  end

  def make_set(item)
    @map[item] = Node.new(item)
  end

  def find(item)    
    find_node(@map[item]).item
  end

  def unite(item1, item2)
    unite_nodes(@map[item1], @map[item2])
  end

  def find_node(node)
    if node.parent
      node.parent = find_node(node.parent)
    else
      node
    end
  end

  def unite_nodes(node1, node2)
    root1 = find_node(node1)
    root2 = find_node(node2)

    return if root1 == root2

    if root1.rank < root2.rank
      root1.parent = root2
    elsif root2.rank < root1.rank
      root2.parent = root1
    else
      node2.parent = node1
      node1.rank = node1.rank + 1
    end
  end

end

end # module SetUnion
