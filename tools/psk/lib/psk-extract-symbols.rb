#
# PSK - Toolchain: extract-symbols
#
# Tool to extract addresses from a patmos ELF file
#

require "utils.rb"
include PML

# Class to extract symbol addresses from an ELF file
class ExtractSymbols
  def initialize(pml)
    @pml,@text_symbols = pml,{}
  end
  def analyze(elf,options)
    r = IO.popen("#{options.objdump} -t '#{elf}'") do |io|
      io.each_line do |line|
        if record = objdump_extract(line)
          next unless options.text_sections.include?(record.section)
          @text_symbols[record.label]=record.address
        end
      end
    end
    die "The command #{options.objdump} exited with status #{$?.exitstatus}" unless $?.success?
  end
  def update_pml 
    @pml.machine_functions.each do |function|
      addr = @text_symbols[Block.get_label(function['name'],0)] || @text_symbols[function['mapsto']]
      function_descr = "#{function['name']}/#{function['mapsto']}"

      (warn("No symbol for machine function #{function_descr}");next) unless addr

      function.blocks.each do |block|
        if block_addr = @text_symbols[block.label]
          # Migh be different from current addr, as subfunctions require the emmiter
          # to insert additional text between blocks
          addr = block_addr
        end
        block.address = addr
        block.instructions.each do |instruction|
          instruction.address = addr
          addr += instruction['size']
        end
      end
    end
    @pml
  end
  private
  RE_PATMOS_LABEL = %r{
    ( #{RE_HEX}{8} ) # address
    . {9}            # .ignore
    ( \S+ ) \s+      # section
    ( #{RE_HEX}+ ) \s+ # value
    ( \S+ ) # label
  }x
  def objdump_extract(line)
    return nil unless line =~ /\A#{RE_PATMOS_LABEL}$/
    OpenStruct.new(:address => Integer("0x#{$1}"), :section => $2, :value => 3, :label => $4)
  end
end

class ExtractSymbolsTool
  def ExtractSymbolsTool.add_options(opts,options)
      opts.on("--objdump-command FILE", "path to 'patmos-objdump'")   { |f| options.objdump = f }
      opts.on("--text-sections SECTION,..", "list of code sections") { |s| options.text_sections = s.split(/\s*,\s*/) }
  end
  def ExtractSymbolsTool.run(elf, pml, options)
    options.objdump = "patmos-objdump" unless options.objdump
    options.text_sections = [".text"] unless options.text_sections
    extract = ExtractSymbols.new(pml)
    extract.analyze(elf, options)
    extract.update_pml
    pml
  end
end

if __FILE__ == $0
  SYNOPSIS=<<EOF
Extract Symbol Addresses from ELF file.
EOF

  options, args = PML::optparse(1..1, "program.elf", SYNOPSIS, :type => :io) do |o|
    ExtractSymbolsTool.add_options(*o)
  end
  ExtractSymbolsTool.run(args.first, PMLDoc.from_file(options.input), options).dump_to_file(options.output)
end
