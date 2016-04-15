//===-- PatmosMCTargetDesc.cpp - Patmos Target Descriptions -----*- C++ -*-===//
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

#include "PatmosMCTargetDesc.h"
#include "PatmosMCAsmInfo.h"
#include "PatmosTargetStreamer.h"
#include "InstPrinter/PatmosInstPrinter.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"

#define GET_INSTRINFO_MC_DESC
#include "PatmosGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "PatmosGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "PatmosGenRegisterInfo.inc"

using namespace llvm;

static MCAsmInfo *createPatmosMCAsmInfo(const MCRegisterInfo &MRI, const Triple &TT)
{
  return new PatmosMCAsmInfo(TT);
}

static MCInstrInfo *createPatmosMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitPatmosMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createPatmosMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitPatmosMCRegisterInfo(X, Patmos::R1);
  return X;
}

static MCSubtargetInfo *createPatmosMCSubtargetInfo(const Triple &TT, StringRef CPU,
                                                    StringRef FS) {
  if (CPU.empty()) {
    // TODO since we do not create a PatmosSubtarget here (for some registration
    // issues it seems), we need to take care about the default CPU model here
    // as well, otherwise we have no SchedModel.
    CPU = "generic";
  }
  return createPatmosMCSubtargetInfoImpl(TT, CPU, FS);
}

static MCCodeGenInfo *createPatmosMCCodeGenInfo(const Triple &TT, Reloc::Model RM,
                                                CodeModel::Model CM,
                                                CodeGenOpt::Level L) {
  MCCodeGenInfo *X = new MCCodeGenInfo();
  if (CM == CodeModel::Default) {
    CM = CodeModel::Small;
  }
  X->initMCCodeGenInfo(RM, CM, L);
  return X;
}

static MCTargetStreamer *createPatmosAsmTargetStreamer(MCStreamer &S,
                                                       formatted_raw_ostream &OS,
                                                       MCInstPrinter *InstPrint,
                                                       bool isVerboseAsm)
{
  return new PatmosTargetAsmStreamer(S, OS);
}


static MCTargetStreamer *createPatmosObjectTargetStreamer(MCStreamer &S,
                                                          const MCSubtargetInfo &STI)
{
  return new PatmosTargetELFStreamer(S);
}

static MCInstPrinter *createPatmosMCInstPrinter(const Triple &TT,
                                                unsigned SyntaxVariant,
                                                const MCAsmInfo &MAI,
                                                const MCInstrInfo &MII,
                                                const MCRegisterInfo &MRI) {
  return new PatmosInstPrinter(MAI, MII, MRI);
}

extern "C" void LLVMInitializePatmosTargetMC() {
  // Register the MC asm info.
  TargetRegistry::RegisterMCAsmInfo(ThePatmosTarget,
                                    createPatmosMCAsmInfo);

  // Register the MC codegen info.
  TargetRegistry::RegisterMCCodeGenInfo(ThePatmosTarget,
                                        createPatmosMCCodeGenInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(ThePatmosTarget,
                                      createPatmosMCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(ThePatmosTarget,
                                    createPatmosMCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(ThePatmosTarget,
                                          createPatmosMCSubtargetInfo);

  // Register the MCInstPrinter.
  TargetRegistry::RegisterMCInstPrinter(ThePatmosTarget,
                                        createPatmosMCInstPrinter);

  // Register the MC code emitter
  TargetRegistry::RegisterMCCodeEmitter(ThePatmosTarget,
                                        createPatmosMCCodeEmitter);

  // Register the asm backend
  TargetRegistry::RegisterMCAsmBackend(ThePatmosTarget,
                                       createPatmosAsmBackend);

  // Register the asm streamer
  TargetRegistry::RegisterAsmTargetStreamer(ThePatmosTarget,
                                            createPatmosAsmTargetStreamer);

  // Register the object streamer
  TargetRegistry::RegisterObjectTargetStreamer(ThePatmosTarget,
                                               createPatmosObjectTargetStreamer);

}
