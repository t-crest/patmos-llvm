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
  // This table *must* be in the save order of
  // MCFixupKindInfo Infos[Patmos::NumTargetFixupKinds]
  // in PatmosAsmBackend.cpp.
  //
  enum Fixups {
    // Branch fixups resulting in R_PATMOS_16.
    fixup_Patmos_16 = FirstTargetFixupKind,

    // Pure 32 bit data fixup resulting in - R_PATMOS_32.
    fixup_Patmos_32,

    // Full 32 bit data relative data fixup resulting in - R_PATMOS_REL32.
    fixup_Patmos_REL32,

    // Jump 26 bit fixup resulting in - R_PATMOS_26.
    fixup_Patmos_26,

    // Pure upper 16 bit fixup resulting in - R_PATMOS_HI16.
    fixup_Patmos_HI16,

    // Pure lower 16 bit fixup resulting in - R_PATMOS_LO16.
    fixup_Patmos_LO16,

    // 16 bit fixup for GP offest resulting in - R_PATMOS_GPREL16.
    fixup_Patmos_GPREL16,

    // 16 bit literal fixup resulting in - R_PATMOS_LITERAL.
    fixup_Patmos_LITERAL,

    // Global symbol fixup resulting in - R_PATMOS_GOT16.
    fixup_Patmos_GOT_Global,

    // Local symbol fixup resulting in - R_PATMOS_GOT16.
    fixup_Patmos_GOT_Local,

    // PC relative branch fixup resulting in - R_PATMOS_PC16.
    fixup_Patmos_PC16,

    // resulting in - R_PATMOS_CALL16.
    fixup_Patmos_CALL16,

    // resulting in - R_PATMOS_GPREL32.
    fixup_Patmos_GPREL32,

    // resulting in - R_PATMOS_SHIFT5.
    fixup_Patmos_SHIFT5,

    // resulting in - R_PATMOS_SHIFT6.
    fixup_Patmos_SHIFT6,

    // Pure 64 bit data fixup resulting in - R_PATMOS_64.
    fixup_Patmos_64,

    // resulting in - R_PATMOS_TLS_GD.
    fixup_Patmos_TLSGD,

    // resulting in - R_PATMOS_TLS_GOTTPREL.
    fixup_Patmos_GOTTPREL,

    // resulting in - R_PATMOS_TLS_TPREL_HI16.
    fixup_Patmos_TPREL_HI,

    // resulting in - R_PATMOS_TLS_TPREL_LO16.
    fixup_Patmos_TPREL_LO,

    // resulting in - R_PATMOS_TLS_LDM.
    fixup_Patmos_TLSLDM,

    // resulting in - R_PATMOS_TLS_DTPREL_HI16.
    fixup_Patmos_DTPREL_HI,

    // resulting in - R_PATMOS_TLS_DTPREL_LO16.
    fixup_Patmos_DTPREL_LO,

    // PC relative branch fixup resulting in - R_PATMOS_PC16
    fixup_Patmos_Branch_PCRel,

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
} // namespace Patmos
} // namespace llvm


#endif // LLVM_PATMOS_PATMOSFIXUPKINDS_H
