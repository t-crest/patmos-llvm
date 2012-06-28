//===-- PatmosFixupKinds.h - Patmos Specific Fixup Entries ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_PATMOS_PATMOSFIXUPKINDS_H
#define LLVM_PATMOS_PATMOSFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace Patmos {
  // Although most of the current fixup types reflect a unique relocation
  // one can have multiple fixup types for a given relocation and thus need
  // to be uniquely named.
  //
  // This table *must* be in the same order of
  // MCFixupKindInfo Infos[Patmos::NumTargetFixupKinds]
  // in PatmosAsmBackend.cpp.
  //
  enum Fixups {
    /// Memory offset, 7 bit signed immediate byte offset, resulting in R_PATMOS_MEM_ABS
    FK_Patmos_BO_7 = FirstTargetFixupKind,

    /// Memory offset, 7 bit signed immediate half-word offset, resulting in R_PATMOS_MEM_ABS
    FK_Patmos_HO_7,

    /// Memory offset, 7 bit signed immediate word offset, resulting in R_PATMOS_MEM_ABS
    FK_Patmos_WO_7,

    /// ALU 12 bit immediate data/absolute byte address fixup, unsigned, resulting in R_PATMOS_ALUI_ABS.
    FK_Patmos_12,

    /// Call direct fixup, 22bit immediate unsigned absolute word address, resulting in R_PATMOS_PFLB_ABS
    FK_Patmos_22,

    /// Stack control fixup, 22bit immediate unsigned absolute word size, emitted as immediate
    FK_Patmos_stc_22,

    /// 32bit ALU immediate data/absolute byte address, resulting in R_PATMOS_ALUL_ABS
    /// (same as FK_Data_4, but with 4 byte offset)
    FK_Patmos_32,

    /// Cache relative byte addresses, 12 bit, resulting in R_PATMOS_ALUI_FREL
    FK_Patmos_crel_12,

    /// Cache relative word addresses, 22 bit, resulting in R_PATMOS_PFLB_FREL
    FK_Patmos_crel_22,

    /// Cache relative byte addresses, 32 bit, resulting in R_PATMOS_ALUL_FREL
    FK_Patmos_crel_32,

    /// Note: we do not have cache-relative 7bit fixups, memory offsets are never cache-relative

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
} // namespace Patmos
} // namespace llvm


#endif // LLVM_PATMOS_PATMOSFIXUPKINDS_H
