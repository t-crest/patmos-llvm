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


    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
} // namespace Patmos
} // namespace llvm


#endif // LLVM_PATMOS_PATMOSFIXUPKINDS_H
