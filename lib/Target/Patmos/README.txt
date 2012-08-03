//===---------------------------------------------------------------------===//
// Patmos Backend.
//===---------------------------------------------------------------------===//

This is the backend for Patmos, the time-predictable processor core.

This work is funded under the European Union's 7th Framework Programme under grant agreement no.
288008: Time-predictable Multi-Core Architecture for Embedded Systems (T-CREST).

TODO Licence information ?!



ELF File Format
---------------

o) ELF Identification
e_machine: EM_PATMOS = 0xBEEB 

o) ELF Relocation infos
TODO: See PatmosELFObjectWriter::GetRelocType for emitted relocation types for now

o) Other
- ELF Symbol flags
    - Bit 12: ELF_Type_SubFunction, set for symbols which point to the beginning of a (sub) function
      (i.e., the first instruction after the alignment and function size word)



Assembler Syntax
----------------

o) General Instruction Syntax

Each operation is predicated, the predicate register is specified before the operation in
parentheses, e.g. (p1) <instruction>.  If the predicate register is prefixed by a !, it is negated.
If omitted, the predicate defaults (p0), i.e.\ always true.

All register names must be prefixed by $. The instructions use destination before source in the
instructions. Between destination and source a = character must be used instead of a comma.

Immediate values are not prefixed for decimal notation, the usual 0 and 0x formats are accepted for
octal and hexadecimal immediates.  Comments start with the hash symbol # and are considered to the
end of the line. 

For memory operations, the syntax is [$register + offset]. Register or offset can be ommited, in
that case the zero register r0 or an offset of 0 is used.

Labels that are prefixed by .L are local labels.

-- Example --
    # add 42 to contents of r2
    # and store result in r1 (first slot)
    add   $r1 = $r2, $42;
    # if r3 equals 50, set p1 to true
    cmpeq $p1, $r3, 50 
    # if p1 is true, jump to label_1
    ($p1) br .Llabel1 ;; nop 3   # then wait 3 cycles
    # Load the address of a symbol into r2
    li $r2 = .L.str2 ;;
    # perform a memory store and a pred op
    swc [$r31 + 2] = $r3 ; or $p1 = !$p2, $p3
    ...
.Llabel1:
    ...
-- End --

o) Bundles 

A double semi-colon ;; or a newline denotes the end of an instruction. If an instruction contains
two operations, the operations must be separated by a single semi-colon or a single semi-colon
followed by either a newline or a comment. Note that since newline is an instruction separator, the
operation separator must always appear on the same line as the operation for the first slot. 

o) Function Relative Addressing, Function Block Start Markers and Subfunction Calls

TODO!



o) Inline Assembler

Inline assembly syntax is similar to GCC inline assembly. It uses %0, %1, ... as placeholders for
operands. Accepted register constraints are: r or R for any general purpose register,
{<registername>} to use a specific register, i for immediates, or the index of an output register to
assign an input register the same register as the output register.

-- Example --
    int i, j, k;
    asm("mov  $r31 = %1 ; # copy i into r31\n\t"
        "add  %0 = $r5, %2\n\t"
	"bs %3\n\t"	     // call myfunction
	"nop 0 ;; nop 0\n\t" // delay slots
        : "=r" (j)
        : "0" (i), "{r10}" (k), "i" (&myfunction));
-- End --


