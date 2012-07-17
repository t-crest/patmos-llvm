//===---------------------------------------------------------------------===//
// Patmos backend.
//===---------------------------------------------------------------------===//

This is the backend for Patmos, the time-predictable processor core.

This work is funded under the European Union's 7th Framework Programme under 
grant agreement no. 288008: Time-predictable Multi-Core Architecture for 
Embedded Systems (T-CREST).

TODO Licence information ?!


ELF File format
---------------

o) ELF Identification
e_machine: EM_PATMOS = 0xBEEB 

o) ELF Relocation infos
TODO: See PatmosELFObjectWriter::GetRelocType for emitted relocation types for now

o) Other
- ELF Symbol flags
    - Bit 12: ELF_Type_SubFunction, set for symbols which point to the beginning of a (sub) function (i.e., 
      the first instruction after the alignment and function size word)


