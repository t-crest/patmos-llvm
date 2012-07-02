//===-- PatmosELFObjectWriter.cpp - Patmos ELF Writer -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PatmosBaseInfo.h"
#include "MCTargetDesc/PatmosFixupKinds.h"
#include "MCTargetDesc/PatmosMCTargetDesc.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCSection.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ELF.h"
#include "llvm/Support/ErrorHandling.h"
#include <list>

using namespace llvm;
using namespace Patmos;

namespace {

  class PatmosELFObjectWriter : public MCELFObjectTargetWriter {
  public:
    PatmosELFObjectWriter(uint8_t OSABI);

    virtual ~PatmosELFObjectWriter();

    virtual unsigned GetRelocType(const MCValue &Target, const MCFixup &Fixup,
                                  bool IsPCRel, bool IsRelocWithSymbol,
                                  int64_t Addend) const;
    virtual unsigned getEFlags() const;
  };
}

PatmosELFObjectWriter::PatmosELFObjectWriter(uint8_t OSABI)
  // TODO use EM_PATMOS
  : MCELFObjectTargetWriter(false, OSABI, ELF::EM_PATMOS,
                            /*HasRelocationAddend*/ false) {}

PatmosELFObjectWriter::~PatmosELFObjectWriter() {}

unsigned PatmosELFObjectWriter::getEFlags() const {

  return 0;
}


unsigned PatmosELFObjectWriter::GetRelocType(const MCValue &Target,
                                           const MCFixup &Fixup,
                                           bool IsPCRel,
                                           bool IsRelocWithSymbol,
                                           int64_t Addend) const {
  // TODO determine the type of the relocation, use Patmos types

  unsigned Kind = (unsigned)Fixup.getKind();

  switch (Kind) {
  case FK_Patmos_BO_7:
  case FK_Patmos_HO_7:
  case FK_Patmos_WO_7:
    // TODO different relocation types for different shifts
    return ELF::R_PATMOS_MEM_ABS;
  case FK_Patmos_12:
    return ELF::R_PATMOS_ALUI_ABS;
  case FK_Patmos_22:
  // TODO do not emit STC format relocations?
  case FK_Patmos_stc_22:
    return ELF::R_PATMOS_PFLB_ABS;
  case FK_Patmos_32:
    return ELF::R_PATMOS_ALUL_ABS;
  case FK_Patmos_frel_12:
    return ELF::R_PATMOS_ALUI_FREL;
  case FK_Patmos_frel_22:
    return ELF::R_PATMOS_PFLB_FREL;
  case FK_Patmos_frel_32:
    return ELF::R_PATMOS_ALUL_FREL;
  default:
    llvm_unreachable("invalid fixup kind!");
  }
}



MCObjectWriter *llvm::createPatmosELFObjectWriter(raw_ostream &OS,
                                                uint8_t OSABI) {
  MCELFObjectTargetWriter *MOTW = new PatmosELFObjectWriter(OSABI);
  return createELFObjectWriter(MOTW, OS, false);
}
