//===-- PatmosELFWriterInfo.cpp - ELF Writer Info for the Patmos backend --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements ELF writer information for the Patmos backend.
//
//===----------------------------------------------------------------------===//

#include "PatmosELFWriterInfo.h"
#include "PatmosRelocations.h"
#include "llvm/Function.h"
#include "llvm/Support/ELF.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

//===----------------------------------------------------------------------===//
//  Implementation of the PatmosELFWriterInfo class
//===----------------------------------------------------------------------===//

PatmosELFWriterInfo::PatmosELFWriterInfo(TargetMachine &TM)
  : TargetELFWriterInfo(TM.getTargetData()->getPointerSizeInBits() == 64,
                        TM.getTargetData()->isLittleEndian()) {
}

PatmosELFWriterInfo::~PatmosELFWriterInfo() {}

unsigned PatmosELFWriterInfo::getRelocationType(unsigned MachineRelTy) const {
  switch (MachineRelTy) {
  case Patmos::reloc_patmos_frel:
    return ELF::R_MICROBLAZE_64_PCREL;
  case Patmos::reloc_patmos_abs:
    return ELF::R_MICROBLAZE_NONE;
  default:
    llvm_unreachable("unknown patmos machine relocation type");
  }
}

long int PatmosELFWriterInfo::getDefaultAddendForRelTy(unsigned RelTy,
                                                    long int Modifier) const {
  switch (RelTy) {
  case ELF::R_MICROBLAZE_32_PCREL:
    return Modifier - 4;
  case ELF::R_MICROBLAZE_32:
    return Modifier;
  default:
    llvm_unreachable("unknown patmos relocation type");
  }
}

unsigned PatmosELFWriterInfo::getRelocationTySize(unsigned RelTy) const {
  // FIXME: Most of these sizes are guesses based on the name
  switch (RelTy) {
  case ELF::R_MICROBLAZE_32:
  case ELF::R_MICROBLAZE_32_PCREL:
  case ELF::R_MICROBLAZE_32_PCREL_LO:
  case ELF::R_MICROBLAZE_32_LO:
  case ELF::R_MICROBLAZE_SRO32:
  case ELF::R_MICROBLAZE_SRW32:
  case ELF::R_MICROBLAZE_32_SYM_OP_SYM:
  case ELF::R_MICROBLAZE_GOTOFF_32:
    return 32;

  case ELF::R_MICROBLAZE_64_PCREL:
  case ELF::R_MICROBLAZE_64:
  case ELF::R_MICROBLAZE_GOTPC_64:
  case ELF::R_MICROBLAZE_GOT_64:
  case ELF::R_MICROBLAZE_PLT_64:
  case ELF::R_MICROBLAZE_GOTOFF_64:
    return 64;
  }

  return 0;
}

bool PatmosELFWriterInfo::isPCRelativeRel(unsigned RelTy) const {
  // FIXME: Most of these are guesses based on the name
  switch (RelTy) {
  case ELF::R_MICROBLAZE_32_PCREL:
  case ELF::R_MICROBLAZE_64_PCREL:
  case ELF::R_MICROBLAZE_32_PCREL_LO:
  case ELF::R_MICROBLAZE_GOTPC_64:
    return true;
  }

  return false;
}

unsigned PatmosELFWriterInfo::getAbsoluteLabelMachineRelTy() const {
  return Patmos::reloc_patmos_abs;
}

long int PatmosELFWriterInfo::computeRelocation(unsigned SymOffset,
                                                unsigned RelOffset,
                                                unsigned RelTy) const {
  assert((RelTy == ELF::R_MICROBLAZE_32_PCREL ||
          RelTy == ELF::R_MICROBLAZE_64_PCREL) &&
         "computeRelocation unknown for this relocation type");
  return SymOffset - (RelOffset + 4);
}
