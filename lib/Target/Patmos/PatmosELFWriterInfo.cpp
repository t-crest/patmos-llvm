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
  switch ((Patmos::RelocationType)MachineRelTy) {
    case Patmos::reloc_patmos_pflb_abs :
      return ELF::R_PATMOS_PFLB_ABS;
    case Patmos::reloc_patmos_pflb_frel:
      return ELF::R_PATMOS_PFLB_FREL;
    case Patmos::reloc_patmos_alui_abs :
      return ELF::R_PATMOS_ALUI_ABS;
    case Patmos::reloc_patmos_alui_frel:
      return ELF::R_PATMOS_ALUI_FREL;
    case Patmos::reloc_patmos_alul_abs :
      return ELF::R_PATMOS_ALUL_ABS;
    case Patmos::reloc_patmos_alul_frel:
      return ELF::R_PATMOS_ALUL_FREL;
    case Patmos::reloc_patmos_mem_abs  :
      return ELF::R_PATMOS_MEM_ABS;
  }

  llvm_unreachable("unknown patmos machine relocation type");
}

long int PatmosELFWriterInfo::getDefaultAddendForRelTy(unsigned RelTy,
                                                      long int Modifier) const {
  switch (RelTy) {
    case ELF::R_PATMOS_PFLB_FREL:
    case ELF::R_PATMOS_ALUI_FREL:
    case ELF::R_PATMOS_ALUL_FREL:
      return Modifier - 4;
    case ELF::R_PATMOS_PFLB_ABS:
    case ELF::R_PATMOS_ALUI_ABS:
    case ELF::R_PATMOS_ALUL_ABS:
    case ELF::R_PATMOS_MEM_ABS:
      return Modifier;
    default:
      llvm_unreachable("unknown patmos relocation type");
  }
}

unsigned PatmosELFWriterInfo::getRelocationTySize(unsigned RelTy) const {
  switch (RelTy) {
    case ELF::R_PATMOS_PFLB_ABS:
    case ELF::R_PATMOS_PFLB_FREL:
      return 22;
    case ELF::R_PATMOS_ALUI_ABS:
    case ELF::R_PATMOS_ALUI_FREL:
      return 12;
    case ELF::R_PATMOS_ALUL_ABS:
    case ELF::R_PATMOS_ALUL_FREL:
      return 32;
    case ELF::R_PATMOS_MEM_ABS:
      return 7;
    default:
      llvm_unreachable("unknown patmos relocation type");
  }

  return 0;
}

bool PatmosELFWriterInfo::isPCRelativeRel(unsigned RelTy) const {
  switch (RelTy) {
    case ELF::R_PATMOS_PFLB_FREL:
    case ELF::R_PATMOS_ALUI_FREL:
    case ELF::R_PATMOS_ALUL_FREL:
      return true;
    case ELF::R_PATMOS_PFLB_ABS:
    case ELF::R_PATMOS_ALUI_ABS:
    case ELF::R_PATMOS_ALUL_ABS:
    case ELF::R_PATMOS_MEM_ABS:
      return false;
    default:
      llvm_unreachable("unknown patmos relocation type");
  }

  return false;
}

unsigned PatmosELFWriterInfo::getAbsoluteLabelMachineRelTy() const {
  return Patmos::reloc_patmos_alul_abs;
}

long int PatmosELFWriterInfo::computeRelocation(unsigned SymOffset,
                                                unsigned RelOffset,
                                                unsigned RelTy) const {
  // TODO: handle most *FREL here
  assert(false && "computeRelocation unknown for this relocation type");
  return SymOffset - (RelOffset + 4);
}
