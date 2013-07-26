#
# platin toolkit
#
# The pml tool is used to
# - validate PML files
# - merge PML files into a single file
# - print statistics for a PML file
# - visualize PML files
#
require 'set'
require 'core/pml'
require 'core/utils'
require 'core/vcfg'
include PML

begin
  require 'kwalify'
rescue Exception => details
  warn "Failed to load library kwalify"
  info "  ==> gem1.9 install kwalify"
  die "Failed to load required ruby libraries"
end

class PMLTool

  def PMLTool.run(pml,options)
    if(options.validate)
      schema_file = options.pml_schema_file
      unless schema_file
        platin_lib_dir = File.dirname(File.dirname(File.absolute_path(__FILE__)))
        schema_file = File.join(platin_lib_dir,"core","pml.yml")
      end
      schema = YAML.load_file(schema_file)
      ## create validator
      validator = Kwalify::Validator.new(schema)
      ## validate
      validation_errors = validator.validate(pml.data)

      ## show errors
      if validation_errors && !validation_errors.empty?
        for e in validation_errors
          warn "[#{e.path}] #{e.message}"
        end
      end
      die("Invalid PML file") unless validation_errors.empty?
      info("No validation errors")
    end
    if(options.stats)
      stats = {}
      stats['machine code functions'] = pml.machine_functions.length
      stats['bitcode functions'] = pml.bitcode_functions.length
      stats['timing entries'] = pml.timing.length
      stats['flowfacts'] = pml.flowfacts.length
      pml.flowfacts.stats(pml).each { |level,by_group|
        by_group.each { |group, by_class|
          next if group == :cnt
          stats["flowfacts/#{level}/#{group}"] = by_class[:cnt]
        }
      }
      statistics("PML", stats)
    end
    pml
  end
  def PMLTool.add_config_options(opts)
    opts.on("--schema FILE", "PML schema") { |f| opts.options.pml_schema_file = f }
  end
  def PMLTool.add_options(opts)
    PMLTool.add_config_options(opts)
    opts.writes_pml # output option => merge
    opts.on("--validate", "validate PML file") { opts.options.validate=true }
  end
end

if __FILE__ == $0
  synopsis="Validate, inspect and/or merge PML documents"
  options, args = PML::optparse([], "", synopsis) do |opts|
    opts.needs_pml
    PMLTool.add_options(opts)
  end
  updated_pml = PMLTool.run(PMLDoc.from_files(options.input), options)
  updated_pml.dump_to_file(options.output) if options.output
end
