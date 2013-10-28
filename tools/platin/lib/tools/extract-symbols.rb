#
# platin toolchain: extract-symbols
#
# Tool to extract addresses from a ELF file
#
require 'platin'
include PML

# Class to extract symbol addresses from an ELF file
class ExtractSymbols
  attr_reader :stats_address_count
  def initialize(pml, options)
    @pml,@options = pml, options
    @text_symbols = {}
    @stats_address_count = 0
    @instruction_addresses = {}
  end
  def add_symbol(label,address)
    @text_symbols[label]=address
    @stats_address_count += 1
  end
  def add_instruction_address(label,index,address)
    (@instruction_addresses[label]||={})[index]=address
  end
  def analyze
    elf = @options.binary_file
    if ! File.exist?(elf)
      die "The binary file '#{elf}' does not exist"
    end
    r = IO.popen("#{@options.objdump} -t '#{elf}'") do |io|
      io.each_line do |line|
        if record = objdump_extract(line.chomp)
          next unless @options.text_sections.include?(record.section)
          debug(@options, :elf) {
            "Adding address for label #{record.label}: #{record.address}"
          }
          add_symbol(record.label, record.address)
        end
      end
    end
    die "The objdump command '#{@options.objdump}' exited with status #{$?.exitstatus}" unless $?.success?

    # Run platform-specific extractor, if available
    # Computes instruction_addresses
    @pml.arch.extract_symbols(self, @pml, @options) if @pml.arch.respond_to?(:extract_symbols)

    statistics("EXTRACT","extracted addresses" => stats_address_count) if @options.stats
    self
  end
  def update_pml
    @pml.machine_functions.each do |function|
      addr = @text_symbols[function.label] || @text_symbols[function.blocks.first.label]
      (warn("No symbol for machine function #{function.to_s}");next) unless addr
      ins_index = 0
      function.blocks.each do |block|
        if block_addr = @text_symbols[block.label]
          # Migh be different from current addr, as subfunctions require the emitter
          # to insert additional text between blocks.
          addr = block_addr
        elsif ! @instruction_addresses[function.label]
          if @instruction_addresses.empty?
            die("There is no symbol for basic block #{block.label} in the binary")
          else
            die("There is no symbol for #{block.label}, and no instruction addresses for function #{function.label} are available")
          end
        elsif ins_addr = @instruction_addresses[function.label][ins_index]
          warn("Heuristic found wrong address for #{block}: #{addr}, not #{ins_addr}") if addr != ins_addr
          addr = ins_addr
        else
          warn("No symbol for basic block #{block}")
        end
        block.address = addr
        block.instructions.each do |instruction|
          # This might be necessary for von-Neumann architectures
          # (e.g., for ARMs BR_JTm instruction, where PML does not provide a size)
          if ins_addr = (@instruction_addresses[function.label]||{})[ins_index]
            warn("Heuristic found wrong address: #{instruction}: #{addr}, not #{ins_addr}") if addr != ins_addr
            addr = ins_addr
          elsif instruction.size == 0
            debug(@options,:elf) { "Size 0 for instruction #{instruction}" }
          end
          instruction.address = addr
          addr += instruction.size
          ins_index += 1
        end
      end
    end
    @pml
  end
  private
  RE_OBJDUMP_LABEL = %r{
    ( #{RE_HEX}{8} ) # address
    . {9}            # .ignore
    ( \S+ ) \s+      # section
    ( #{RE_HEX}+ ) \s+ # value
    ( \S+ ) # label
  }x
  def objdump_extract(line)
    return nil unless line =~ /\A#{RE_OBJDUMP_LABEL}$/
    OpenStruct.new(:address => Integer("0x#{$1}"), :section => $2, :value => 3, :label => $4)
  end
end

class ExtractSymbolsTool
  def ExtractSymbolsTool.add_config_options(opts)
    opts.on("--objdump-command FILE", "path to 'llvm-objdump'")   { |f| opts.options.objdump = f }
    opts.on("--text-sections SECTION,..", "list of code sections (=.text)")  { |s| opts.options.text_sections = s.split(/\s*,\s*/) }
    opts.add_check do |options|
      options.objdump = "patmos-llvm-objdump" unless options.objdump
      options.text_sections = [".text"] unless options.text_sections
    end
  end
  def ExtractSymbolsTool.add_options(opts)
    ExtractSymbolsTool.add_config_options(opts)
  end
  def ExtractSymbolsTool.run(pml, options)
    needs_options(options, :objdump, :text_sections, :binary_file)
    ExtractSymbols.new(pml,options).analyze.update_pml
  end
end

if __FILE__ == $0
  SYNOPSIS=<<EOF
Extract Symbol Addresses from ELF file. It is possible to specify the same file
for input and output; as long as the ELF file does not change, this is an
idempotent transformation.
EOF

  options, args = PML::optparse([:binary_file], "program.elf", SYNOPSIS) do |opts|
    opts.needs_pml
    opts.writes_pml
    ExtractSymbolsTool.add_options(opts)
  end
  ExtractSymbolsTool.run(PMLDoc.from_files(options.input), options).dump_to_file(options.output)
end
