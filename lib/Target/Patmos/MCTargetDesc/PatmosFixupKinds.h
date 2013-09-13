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
    /// Memory offset, 7 bit unsigned immediate byte offset, resulting in R_PATMOS_MEMB_ABS
    FK_Patmos_BO_7 = FirstTargetFixupKind,

    /// Memory offset, 7 bit unsigned immediate half-word offset, resulting in R_PATMOS_MEMH_ABS
    FK_Patmos_HO_7,

    /// Memory offset, 7 bit unsigned immediate word offset, resulting in R_PATMOS_MEMW_ABS
    FK_Patmos_WO_7,

    /// ALU 12 bit immediate data/absolute byte address fixup, unsigned, resulting in R_PATMOS_ALUI_ABS.
    FK_Patmos_abs_ALUi,

    /// Call direct fixup, 22bit immediate unsigned absolute word address, resulting in R_PATMOS_CFLI_ABS
    FK_Patmos_abs_CFLi,

    /// 32bit ALU immediate data/absolute byte address, resulting in R_PATMOS_ALUL_ABS
    /// (same as FK_Data_4, but with 4 byte offset)
    FK_Patmos_abs_ALUl,

    /// Stack control fixup, 18bit immediate unsigned absolute word size, emitted as immediate
    FK_Patmos_stc,

    /// PC relative word addresses, 22 bit immediate, resulting in R_PATMOS_CFLI_PCREL
    FK_Patmos_PCrel,

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };

  static inline bool isPCRELFixupKind(unsigned FixupKind) {
    return FixupKind == FK_Patmos_PCrel;
  }

} // namespace Patmos
} // namespace llvm


#endif // LLVM_PATMOS_PATMOSFIXUPKINDS_H
