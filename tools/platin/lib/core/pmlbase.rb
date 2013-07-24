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
    def ProgramInfoObject.attributes_from_pml(data)
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
    attr_reader :data, :triple, :arch, :bitcode_functions,:machine_functions,:relation_graphs,:flowfacts,:timing

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
      @data['timing'] ||= []
      @timing = TimingList.from_pml(self, @data['timing'])
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

    def try
      backup = flowfacts,timing
      begin
        @flowfacts = flowfacts.dup
        @timing = timing.dup
        r = yield
      ensure
        @flowfacts, @timing = backup
      end
      r
    end

    def to_s
      "PMLDoc{bitcode-functions: |#{bitcode_functions.length}|, machine-functions: |#{machine_functions.length}"+
        ", flowfacts: |#{flowfacts.length}|}, timing: |#{timing.length}|"
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
      final = @data.clone
      final.delete("flowfacts") if @data["flowfacts"] == []
      final.delete("timing") if @data["timing"] == []
      io.write(YAML::dump(final))
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
  # By default, it returns to_pml (cached in the instance variable @data)
  class PMLObject
    # The PML data corresponding to the object
    def data
      return @data if @data
      @data = to_pml
    end
    def to_pml
      return @data if @data
      raise Exception.new("#{self.class}: to_pml not implemented and not data available")
    end
    protected

    # Set data (usually during construction)
    # If not data is available, use dat=nil
    def set_data(dat)
      @data = dat
    end
  end

  # A PML list is a list of PML objects, along with a data representation
  # It provides indexing facilities for subclasses
  class PMLList < PMLObject
    attr_reader :list
    def to_s
      list.to_s
    end
    def to_pml
      list.map { |t| t.to_pml }
    end
    # delegator to list (which should be frozen)
    def method_missing(method, *args, &block)
      list.send(method, *args, &block)
    end
    def lookup(dict,key,name,error_if_missing=true)
      v = dict[key]
      if ! v && error_if_missing
        raise Exception.new("#{self.class}#by_#{name}: No object with key '#{key}' in #{dict.inspect}")
      end
      v
    end
    def add_lookup(dict,key,val,name,opts={})
      return if ! key && opts[:ignore_if_missing]
      if dict[key]
        raise Exception.new("#{self.class}#by_#{name}: Duplicate object with key #{key}: #{val} and #{dict[key]}")
      end
      dict[key] = val
    end
  end

  # Lists where elements can be queried by name and qualified name
  module NameIndexList
    def by_name(name, error_if_missing = true)
      build_name_index unless @named
      lookup(@named, name, "name", error_if_missing)
    end
    def by_qname(name, error_if_missing = true)
      build_name_index unless @named
      lookup(@qnamed, name, "qname", error_if_missing)
    end
    def build_name_index
      @named, @qnamed = {}, {}
      list.each do |v|
        add_lookup(@named, v.name, v, "name")
        add_lookup(@qnamed, v.qname, v, "qname")
      end
    end
  end

end # end module pml
