//===-- PatmosRelocations.h - Patmos Code Relocations ---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Patmos target-specific relocation types
// (for relocation-model=static).
//
//===----------------------------------------------------------------------===//

#ifndef PATMOSRELOCATIONS_H_
#define PATMOSRELOCATIONS_H_

#include "llvm/CodeGen/MachineRelocation.h"

namespace llvm {
  namespace Patmos{
    enum RelocationType {
      /// reloc_patmos_pfl_abs - absolute relocation for branches/calls (PFLb 
      /// format)
      reloc_patmos_pflb_abs = 1,

      /// reloc_patmos_pfl_frel - function relative relocation for branches 
      /// (PFLb format)
      reloc_patmos_pflb_frel = 2,

      /// reloc_patmos_alui_abs - absolute relocation for ALUi instructions
      reloc_patmos_alui_abs = 3,

      /// reloc_patmos_alui_frel - function relative relocation for ALUi
      /// instructions
      reloc_patmos_alui_frel = 4,

      /// reloc_patmos_alul_abs - absolute relocation for ALUl instructions
      reloc_patmos_alul_abs = 5,

      /// reloc_patmos_alul_frel - function relative relocation for ALUl
      /// instructions
      reloc_patmos_alul_frel = 6,

      /// reloc_patmos_mem_abs - absolute relocation for LDT/STT instructions
      reloc_patmos_mem_abs = 7
    };
  }
}

#endif /* PATMOSRELOCATIONS_H_ */
