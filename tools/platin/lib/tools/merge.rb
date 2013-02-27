#
# PSK toolset
#
# Merge a list of PML files into one
#
# The real work is done in the PML library,
# this is just a simple wrapper
#
require 'platin'
include PML

class MergeTool
  def MergeTool.run(files)
    stream = []
    files.each do |f|
      data = if f.kind_of?(String)
               File.open(f) { |fh| YAML::load_stream(fh) }
             else
               YAML::load_stream(f)
             end
      data = data.documents if data.respond_to?(:documents) # ruby 1.8 compat
      stream.concat(data)
    end
    PMLDoc.new(stream)
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Merges a stream of YAML documents into one document (if version
and architecture are compatible)
EOF

  options, args = PML::optparse(nil, "[file.pml...]", SYNOPSIS) do |opts|
    opts.writes_pml
  end
  args = [ $< ] if args.empty?
  MergeTool.run(args).dump_to_file(options.output)
end
