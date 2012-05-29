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
      // reloc_patmos_abs - absolute relocation for branches.
      reloc_patmos_abs = 1,

      // reloc_patmos_12 - lower 12 bits of the address
      reloc_patmos_12 = 2,

      // reloc_patmos_22 - lower 22 bits of the address
      reloc_patmos_22 = 3,

      // reloc_patmos_frel - lower 32 bits of the address, function relative
      reloc_patmos_frel = 4,

      // reloc_patmos_frel22 - lower 22 bits of the address, function relative
      reloc_patmos_frel22 = 5
    };
  }
}

#endif /* PATMOSRELOCATIONS_H_ */
