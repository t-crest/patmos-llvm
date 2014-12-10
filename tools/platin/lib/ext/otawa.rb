#
# The *platin* toolkit
#
# Bridge to OTAWA WCET analyser and OSX generator.
#

require 'platin'
require "rexml/document"
require "rexml/formatters/transitive"

module PML

# option extensions for ffx
class OptionParser
  def otawa_platform_file(mandatory=true)
    self.on("--otawa-platform-file FILE", "Platform description file for OTAWA") { |f| options.otawa_platform_file = f }
    self.add_check { |options| die_usage "Option --otawa-platform-file is mandatory" unless options.otawa_platform_file } if mandatory
  end
  def otawa_report_file(mandatory=true)
    self.on("--otawa-report-file FILE", "Filename for OTAWA's result file") {
      |f| options.otawa_report_file = f
    }
    self.add_check { |options| die_usage "Option --otawa-report-file is mandatory" unless options.otawa_report_file } if mandatory
  end
end

# Features not supported by the Otawa module
class OtawaUnsupportedFeatureException < Exception
  def initialize(msg)
    super(msg)
  end
end


# class to export PML machine information to OSX
class OSXExporter

  attr_reader :options

  def initialize(pml, options)
    @pml = pml
    @options = options
  end

#  def export_machine_description
#    @pml.arch.config.caches.each { |cache|
#      case cache.name
#      when 'data-cache'
#        gen_fact("cache data size=#{cache.size}, associativity=#{cache.associativity}, line-size=#{cache.line_size},"+
#                 "policy=#{cache.policy.upcase}, may=chaos", "PML machine configuration")
#      when 'instruction-cache'
#        gen_fact("cache code size=#{cache.size}, associativity=#{cache.associativity}, line-size=#{cache.line_size},"+
#                 "policy=#{cache.policy.upcase}, may=chaos", "PML machine configuration")
#      when 'method-cache' # new in aiT version >= 205838
#        gen_fact("global method_cache_block_size=#{cache.block_size}","PML machine configuration")
#        mcache_policy, mcache_assoc =  cache.policy.upcase, cache.associativity
#        max_mcache_assoc = MAX_METHODCACHE_ASSOCIATIVITY[mcache_policy.upcase]
#        if mcache_assoc > max_mcache_assoc
#          warn("aiT: method cache with policy #{mcache_policy} and associativity > #{max_mcache_assoc}"+
#               " is not supported by aiT (assuming associativity #{max_mcache_assoc})")
#          mcache_assoc = max_mcache_assoc
#        end
#        line_size = 4 # template by absint
#        cache_size = line_size * mcache_assoc
#        gen_fact("cache code size=#{cache_size}, associativity=#{mcache_assoc}, line-size=#{line_size},"+
#                 "policy=#{cache.policy.upcase}, may=chaos", "PML machine configuration")
#      when 'stack-cache'
#        # always enabled (new in aiT version >= 205838)
#        if cache.size != 1024
#          warn("aiT: currently not possible to configure stack cache size different from 1024 bytes")
#        end
#      end
#    }
#
#    @pml.arch.config.memory_areas.each { |area|
#      kw = if area.type == 'code' then 'code' else 'data' end
#      if area.memory.transfer_size != 8
#        warn("aiT: currently the only valid size for one burst is 8 bytes")
#      end
#      tt_read_first_beat = area.memory.read_latency + area.memory.read_transfer_time
#      tt_write_first_beat = area.memory.write_latency + area.memory.write_transfer_time
#      properties = [ "#{kw} read transfer-time = [#{tt_read_first_beat},#{area.memory.read_transfer_time}]" ]
#      if area.cache
#        # Changed in aiT version 205838 (should not be specified)
#        #          properties.push("#{kw} cached")
#      elsif area.type == 'scratchpad'
#        properties.push("#{kw} locked")
#      end
#      if area.type != 'code'
#        properties.push("#{kw} write time = #{tt_write_first_beat}")
#      end
#      adress_range = area.address_range
#      address_range = ValueRange.new(0,0xFFFFFFFF,nil) unless address_range
#      gen_fact("area #{address_range.to_ffx} access #{properties.join(", ")}",
#               "PML machine configuration")
#    }
#  end

  def add_bank(banks, area)
    tt_read_first_beat = area.memory.read_latency + area.memory.read_transfer_time
    tt_write_first_beat = area.memory.write_latency + area.memory.write_transfer_time

    address_range = area.address_range
    address_range = ValueRange.new(0,0xFFFFFFFF,nil) unless address_range

    add_element(banks, "bank") { |bank|
      bank << rexml_str("name", "RAM")
      
      bank << rexml_bool("cached", true) if area.cache
    }
  end

  def add_memory(platform)
    add_element(platform, "memory") { |mem|
      add_element(mem, "banks") { |banks|
        @pml.arch.config.memory_areas.each { |area|
	  add_bank(banks, area)
	}
      }
    }
  end

  def add_caches(platform)
    add_element(platform, "caches") { |caches|

    }
  end

  def export_platform(outfile)
    # There is probably a better way to do this .. e.g., use a template file.

    doc = REXML::Document.new "<?xml version=\"1.0\"?><platform></platform>"
    platform = doc.root
    add_memory(platform)
    add_caches(platform)

#    add_element(project, "options") { |proj_options|
#      add_element(proj_options, "analyses_options") { |an_options|
#        an_options << rexml_bool("extract_annotations_from_source_files", true)
#        an_options << rexml_bool("xml_call_graph", true)
#        an_options << rexml_bool("xml_show_per_context_info", true)
#        an_options << rexml_bool("persistence_analysis", true)
#        an_options << rexml_bool("xml_wcet_path", true)
#        an_options << rexml_bool("xml_non_wcet_cycles", true)
#        an_options << rexml_str("path_analysis_variant", "Prediction file based (ILP))")
#      }
#      add_element(proj_options, "general_options") { |gen_options|
#        gen_options << rexml_str("include_path",".")
#      }
#      if arch_el = @pml.arch.config_for_apx(@options)
#        proj_options << arch_el
#      end
#    }
#    add_element(project, "files") { |files|
#      [ ['executables', binary],  ['ffx',ffxfile],
#        ['xml_results', results], ['report',report] ].each { |k,v|
#        files << rexml_file(k,v)
#      }
#    }

    REXML::Formatters::Transitive.new( 2, false ).write(doc, outfile)
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

class OtawaAnalyzer

end

class OtawaImporter
  attr_reader :pml, :options

  def initialize(pml, options)
    @pml, @options = pml, options
  end

  def run
    analysis_entry  = pml.machine_functions.by_label(options.analysis_entry, true)

    # TODO Implement .. 


  end
end


end # end module PML

