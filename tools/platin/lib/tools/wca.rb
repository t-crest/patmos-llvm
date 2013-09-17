#!/usr/bin/env ruby
#
# platin tool set
#
# "Inhouse" IPET-based WCET analysis

require 'platin'
require 'analysis/wca'
include PML

class WcaTool

  def WcaTool.add_config_options(opts)
  end

  def WcaTool.add_options(opts)
    WcaTool.add_config_options(opts)
    opts.analysis_entry
    opts.flow_fact_selection
    opts.callstring_length
    opts.calculates_wcet('wca-unknown')
  end

  def WcaTool.run(pml,options)
    needs_options(options, :analysis_entry, :flow_fact_selection, :flow_fact_srcs, :timing_output)
    wca = WCA.new(pml, options)
    report = wca.analyze(options.analysis_entry)
    pml.timing.add(report)
    pml
  end
end

if __FILE__ == $0
SYNOPSIS=<<EOF if __FILE__ == $0
Calculate WCET using lp_solve and a simple timing model
EOF
  options, args = PML::optparse(0, "", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    WcaTool.add_options(opts)
  end
  WcaTool.run(PMLDoc.from_files(options.input), options).dump_to_file(options.output)
end
