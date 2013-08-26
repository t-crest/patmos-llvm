#
# PLATIN tool set
#
# PML data format classes
#
# Provide smart accessors, caching, etc.
#
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

  # class providing convenient accessors and additional program information derived
  # from PML files
  class PMLDoc
    attr_reader :data, :triple, :arch
    attr_reader :bitcode_functions,:machine_functions,:relation_graphs
    attr_reader :flowfacts,:valuefacts,:timing

    # constructor expects a YAML document or a list of YAML documents
    def initialize(stream)
      stream = [stream] unless stream.kind_of?(Array)
      if stream.length == 1
        @data = stream[0]
      else
        @data = PMLDoc.merge_stream(stream)
      end
      @triple = @data['triple'].split('-')
      @arch = Architecture.from_triple(triple)
      @bitcode_functions = FunctionList.new(@data['bitcode-functions'] || [], :labelkey => 'name')
      @machine_functions = FunctionList.new(@data['machine-functions'] || [], :labelkey => 'mapsto')
      @relation_graphs   = RelationGraphList.new(@data['relation-graphs'] || [],
                                                 @bitcode_functions, @machine_functions)
      @data['flowfacts'] ||= []
      @flowfacts = FlowFactList.from_pml(self, @data['flowfacts'])
      @data['valuefacts'] ||= []
      @valuefacts = ValueFactList.from_pml(self, @data['valuefacts'])
      @data['timing'] ||= []
      @timing = TimingList.from_pml(self, @data['timing'])
    end
    def valuefacts
      @valuefacts
    end
    def functions_for_level(level)
      if level == 'bitcode'
        bitcode_functions
      elsif level == 'machinecode'
        machine_functions
      else
        raise Exception.new("Unsupported representation level: #{level}")
      end
    end

    def clone_empty
      data = {}
      data['format'] = @data['format']
      data['triple'] = @data['triple']
      PMLDoc.new(data)
    end

    #
    # used if some modifications to the PML database should not become permanent
    # saves the specified sections before yielding, and restores them afterwards
    def with_temporary_sections(temporary_sections = [:flowfacts, :valuefacts, :timing])
      backup = temporary_sections.map { |s| self.send(s) }
      begin
        temporary_sections.map { |s|
          instance_variable_set("@#{s}", Marshal.load(Marshal.dump(self.send(s))))
        }
        r = yield
      ensure
        temporary_sections.zip(backup).each { |s,b|
          instance_variable_set("@#{s}",b)
        }
      end
      r
    end

    def to_s
      sprintf("PMLDoc{bitcode-functions: |%d|, machine-functions: |%d|"+
              ", flowfacts: |%s|, valuefacts: |%d|, timings: |%d|",
              bitcode_functions.length, machine_functions.length,
              flowfacts.length,valuefacts.length,timing.length)
    end

    def dump_to_file(filename)
      if filename.nil? || filename == '-'
        dump($>)
      else
        File.open(filename, "w") do |fh|
          dump(fh)
        end
      end
    end

    def dump(io)
      final = deep_data_clone # eliminate sharing to enable YAML import in LLVM
      final.delete("flowfacts") if @data["flowfacts"] == []
      final.delete("valuefacts") if @data["valuefacts"] == []
      final.delete("timing") if @data["timing"] == []
      io.write(YAML::dump(final))
    end

    def deep_data_clone
      cloned_data = @data.dup
      worklist = [cloned_data]
      while ! worklist.empty?
        d = worklist.pop
        if d.kind_of?(Hash)
          d.each { |k,v|
            # compounds are always sequences (Array) or mappings (Hash)
            if v.kind_of?(Hash) || v.kind_of?(Array)
              d[k] = v_copy = v.dup
              worklist.push(v_copy)
            end
          }
        elsif d.kind_of?(Array)
          d.each_with_index { |v,k|
            if v.kind_of?(Hash) || v.kind_of?(Array)
              d[k] = v_copy = v.dup
              worklist.push(v_copy)
            end
          }
        else
          assert("Internal error in deep_data_clone: non-compound in worklist") { false }
        end
      end
      cloned_data
    end

    def machine_code_only_functions
      %w{_start _exit exit abort __ashldi3 __adddf3 __addsf3 __divsi3 __udivsi3 __divdf3 __divsf3 __eqdf2 __eqsf2 __extendsfdf2} +
        %w{__fixdfdi __fixdfsi __fixsfdi __fixsfsi __fixunsdfdi __fixunsdfsi __fixunssfdi __fixunssfsi __floatdidf __floatdisf} +
        %w{__floatsidf __floatsisf __floatundidf __floatundisf __floatunsidf __floatunsisf __gedf2 __gesf2 __gtdf2 __gtsf2} +
        %w{__ledf2 __lesf2 __lshrdi3 __ltdf2 __ltsf2 __muldf3 __mulsf3 __nedf2 __nesf2 __subdf3 __subsf3 __truncdfsf2 __unorddf2 __unordsf2} +
        %w{memcpy memmove memset}
    end

    def PMLDoc.from_files(filenames)
      streams = filenames.inject([]) { |list,f|
        begin
          fstream = File.open(f) { |fh|
            stream = YAML::load_stream(fh)
            stream.documents if stream.respond_to?(:documents) # ruby 1.8 compat
            stream
          }
          list + fstream
        rescue Exception => detail
          die("Failed to load PML document: #{detail}")
        end
      }
      PMLDoc.new(streams)
    end

    def PMLDoc.merge_stream(stream)
      merged_doc = {}
      stream.each do |doc|
        doc.each do |k,v|
          if(v.kind_of? Array)
            (merged_doc[k]||=[]).concat(v)
          elsif(! merged_doc[k])
            merged_doc[k] = doc[k]
          elsif(merged_doc[k] != doc[k])
            die "Mismatch in non-list attribute #{k}: #{merged_doc[k]} and #{doc[k]}"
          end
        end
      end
      merged_doc
    end
  end


  # architectures
  class Architecture
    @@register = {}
    def Architecture.register(archname,klass)
      die("architecture #{archname} already registered to #{@@register[archname]}") if @@register[archname]
      @@register[archname] = klass
    end
    def Architecture.simulator_options(opts)
      opts.on("--trace-file FILE", "FILE generated by architecture simulator") { |f| opts.options.trace_file = f }
      @@register.each { |arch,klass|
        klass.simulator_options(opts)
      }
    end
    def Architecture.from_triple(triple)
      archname = triple.first
      die("unknown architecture #{triple} (#{@@register})") unless @@register[archname]
      @@register[archname].new(triple)
    end
  end

  require 'arch/patmos'
  require 'arch/arm'

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
