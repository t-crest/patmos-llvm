#
# PLATIN tool set
#
# PML data format classes
#
# Provide smart accessors, caching, etc.
#
require 'core/utils'

module PML

  # FIXME: move the utility stuff to a file on its own
  RE_HEX=/[0-9A-Fa-f]/

  # Mixin for entities which are identified by a qualified name (qname), and use this
  # identifier for comparison and hashing
  module QNameObject
    def qname
      assert("QNameObject: @qname not set (fatal)") { @qname }
      @qname
    end
    def ==(other)
      return false if other.nil?
      return false unless other.respond_to?(:qname)
      qname == other.qname
    end
    def eql?(other); self == other ; end
    def hash
      return @hash if @hash
      @hash=qname.hash
    end
    def <=>(other)
      qname <=> other.qname
    end
  end

  #
  # Mixin for entities that provide information from a specific origin and
  # at a specific representation level, stored in (attr_reader) 'attributes'
  #
  module ProgramInfoObject
    def ProgramInfoObject.attribute_list
      %w{origin level}
    end
    def ProgramInfoObject.attributes_from_pml(_,data)
      if data.nil?
        {}
      else
        attrs = {}
        ProgramInfoObject.attribute_list.each { |k| attrs[k] = data[k] if data[k] }
        attrs
      end
    end
    def attributes_to_pml(data)
      attributes.each { |k| data[k] = attributes[k] if attributes[k] }
    end
    def add_attribute(k,v)
      attributes[k] = v
      data[k] = v if data
    end
    def origin
      attributes['origin']
    end
    def origin=(origin)
      add_attribute('origin', origin)
    end
    def level
      attributes['level']
    end
    def level=(level)
      add_attribute('level', level)
    end
  end

  # PML entities provide a method data to access the YAML representation
  # In the constructor, PMLObjects should call set_yaml_repr(yaml_data) if there is
  # an existing YAML representation available.
  # Additionally, every PML object needs to implement to_pml (to generate the YAML representation),
  # and must keep YAML and instance variables in sync at all times.
  class PMLObject

    def data
      # on-demand construction of the YAML representation
      @data = to_pml unless @data
      @data
    end

    def reset_yaml_repr
      @data = nil
    end

    # delete YAML representation when dupping
    def initialize_dup(new_obj)
      super(new_obj)
      new_obj.reset_yaml_repr
    end
    def initialize_clone(new_obj)
      super(new_obj)
      new_obj.reset_yaml_repr
    end

    # avoid recursive calls when printing an object
    # subclasses should override to_s/inspect if needed
    def to_s ; @qname || "#<#{self.class}:#{self.object_id}>" ; end
    def inspect ; to_s ; end

    private

    def to_pml
      # return cached YAML representation, if available
      return @data if @data
      raise Exception.new("#{self.class}: to_pml not implemented")
    end

    # set YAML representation, if not nil
    def set_yaml_repr(dat)
      @data = dat unless dat.nil?
    end
  end

  # A PML list is a list of PML objects, along with a data representation
  # It provides indexing facilities and keeps the list representation in sync
  class PMLList < PMLObject
    include Enumerable
    attr_reader :list
    def to_s
      list.to_s
    end
    def to_pml
      list.map { |t| t.data }
    end
    def add(item)
      list.push(item)
      if @data ; data.push(item.data) ; end
      add_index(item)
    end
    def clear!
      @list = []
      @data = [] if @data
    end
    # basic list operations (delegators)
    def first ; list.first ; end
    def last ; list.last ; end
    def length ; list.length ; end
    def size ; length ; end
    def empty? ; list.empty? ; end
    def [](index) ; list[index]; end
    def each(&block) ; list.each(&block) ; end
    def push(item); add(item); end
    def dup
      self.class.new(@list.dup, data.dup)
    end
    def deep_clone
      self.class.new(@list.map { |item| item.deep_clone }, nil)
    end
    def lookup(dict,key,name,error_if_missing=true)
      v = dict[key]
      if ! v && error_if_missing
        raise Exception.new("#{self.class}#by_#{name}: No object with key '#{key}' in #{dict.inspect}")
      end
      v
    end
    private
    def add_lookup(dict,key,val,name,opts={})
      return if ! key && opts[:ignore_if_missing]
      if dict[key]
        raise Exception.new("#{self.class}#by_#{name}: Duplicate object with key #{key}: #{val} and #{dict[key]}")
      end
      dict[key] = val
    end
  end

  #
  # auto-generate smart lists implementation
  #
  module PMLListGen
    def pml_list(element_type, unique_indices = [], indices = [])
      all_indices = unique_indices + indices
      module_eval %Q$
        def initialize(list, existing_data = nil)
          assert("#{self.class}#initialize: list must not be nil") { list }
          @list = list
          set_yaml_repr(existing_data)
          build_index
        end
        def self.from_pml(ctx,data)
          self.new(data.map { |d| #{element_type}.from_pml(ctx,d) }, data)
        end
        def build_index
          #{all_indices.map { |index| "@index_#{index} = {}"}.join("; ") }
          @list.each { |item| add_index(item) }
        end
        private
        def add_index(item)
          #{(unique_indices.map { |index|
            %Q&
              k = item.send(:#{index})
              if k
                if duplicate = @index_#{index}[k]
                  raise Exception.new("#{self.class}#add_index(\#{item.inspect}): duplicate index #{index}: \#{duplicate.inspect}")
                end
                @index_#{index}[k] = item
              end
            &
           } +
           indices.map { |index|
             "(@index_#{index}[item.send(:#{index})]||=[]).push(item)"
           }).join(";")
          }
        end
      $
      all_indices.each { |index|
         module_eval %Q$
           def by_#{index}(key, error_if_missing = false)
               lookup(@index_#{index}, key, "#{index}", error_if_missing)
           end
         $
      }
    end
    def pml_name_index_list(element_type, unique_indices = [], indices = [])
      pml_list(element_type, [:name,:qname] + unique_indices, indices)
      module_eval %Q!
        def [](arg)
          by_name(arg)
        end
      !
    end
  end
end # end module pml
