//===-- PatmosMCTargetDesc.h - Patmos Target Descriptions -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Patmos specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef _PATMOS_MCTARGETDESC_H_
#define _PATMOS_MCTARGETDESC_H_

#include "llvm/Support/DataTypes.h"

namespace llvm {
class MCAsmBackend;
class MCContext;
class MCCodeEmitter;
class MCInstrInfo;
class MCObjectWriter;
class MCSubtargetInfo;
class Target;
class StringRef;
class raw_ostream;

extern Target ThePatmosTarget;

MCCodeEmitter *createPatmosMCCodeEmitter(const MCInstrInfo &MCII,
                                         const MCSubtargetInfo &STI,
                                         MCContext &Ctx);

MCAsmBackend *createPatmosAsmBackend(const Target &T, StringRef TT);

MCObjectWriter *createPatmosELFObjectWriter(raw_ostream &OS, uint8_t OSABI);
} // End llvm namespace

// Defines symbolic names for Patmos registers.
// This defines a mapping from register name to register number.
#define GET_REGINFO_ENUM
#include "PatmosGenRegisterInfo.inc"

// Defines symbolic names for the Patmos instructions.
#define GET_INSTRINFO_ENUM
#include "PatmosGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "PatmosGenSubtargetInfo.inc"

#endif // _PATMOS_MCTARGETDESC_H_
