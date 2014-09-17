#
# platin tool set
#
# analysis configurations for batch processing
# mostly boring wrappers

require 'core/pmlbase'

module PML # PML namespace

class AnalysisConfigList < PMLList
  extend PMLListGen
  pml_list(:AnalysisConfig,[:name],[:program_entry,:analysis_entry])
end

class AnalysisConfig < PMLObject
  attr_reader :name, :program_entry, :analysis_entry, :tool_configurations
  def initialize(name, program_entry, analysis_entry, tool_configurations, data = nil)
    @name, @program_entry, @analysis_entry, @tool_configurations =
      name, program_entry, analysis_entry, tool_configurations
    set_yaml_repr(data)
  end
  def AnalysisConfig.from_pml(pml, data)
    AnalysisConfig.new(data['name'],data['program-entry'],data['analysis-entry'],
                       ToolConfigList.from_pml(pml, data['tool-configurations'] || []),data)
  end
  def to_pml
    { 'name' => @name, 'program-entry' => @program_entry, 'analysis-entry' => @analysis_entry,
      'tool-configurations' => @tool_configurations }.delete_if { |k,v| v.nil? }
  end
end

class ToolConfigList < PMLList
  extend PMLListGen
  pml_list(:ToolConfig,[:name])
end

class ToolConfig < PMLObject
  attr_reader :name, :configuration, :options
  def initialize(name, configuration, options, data)
    @name, @configuration, @options = name, configuration, options
    set_yaml_repr(data)
  end
  def ToolConfig.from_pml(pml, data)
    ToolConfig.new(data['name'],data['configuration'],data['options'], data)
  end
  def to_pml
    { 'name' => @name, 'configuration' => @configuration, 
      'options' => @options }.delete_if { |k,v| v.nil? }
  end
end

end # module PML
