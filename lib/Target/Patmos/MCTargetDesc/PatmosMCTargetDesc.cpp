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

#define GET_INSTRINFO_MC_DESC
#include "PatmosGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "PatmosGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "PatmosGenRegisterInfo.inc"

using namespace llvm;

static MCAsmInfo *createPatmosMCAsmInfo(const MCRegisterInfo &MRI, StringRef TT)
{
  return new PatmosMCAsmInfo(TT);
}

static MCInstrInfo *createPatmosMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitPatmosMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createPatmosMCRegisterInfo(StringRef TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitPatmosMCRegisterInfo(X, Patmos::R1);
  return X;
}

static MCSubtargetInfo *createPatmosMCSubtargetInfo(StringRef TT, StringRef CPU,
                                                    StringRef FS) {
  MCSubtargetInfo *X = new MCSubtargetInfo();
  if (CPU.empty()) {
    // TODO since we do not create a PatmosSubtarget here (for some registration
    // issues it seems), we need to take care about the default CPU model here
    // as well, otherwise we have no SchedModel.
    CPU = "generic";
  }
  InitPatmosMCSubtargetInfo(X, TT, CPU, FS);
  return X;
}

static MCCodeGenInfo *createPatmosMCCodeGenInfo(StringRef TT, Reloc::Model RM,
                                                CodeModel::Model CM,
                                                CodeGenOpt::Level L) {
  MCCodeGenInfo *X = new MCCodeGenInfo();
  if (CM == CodeModel::Default)
    CM = CodeModel::Small;
  X->InitMCCodeGenInfo(RM, CM, L);
  return X;
}

static MCStreamer *createPatmosMCStreamer(const Target &T, StringRef TT,
                                    MCContext &Ctx, MCAsmBackend &MAB,
                                    raw_ostream &_OS, MCCodeEmitter *_Emitter,
                                    bool RelaxAll, bool NoExecStack)
{
  Triple TheTriple(TT);

  if (TheTriple.isOSDarwin()) {
    llvm_unreachable("Patmos does not support Darwin MACH-O format");
  }

  if (TheTriple.isOSWindows()) {
    llvm_unreachable("Patmos does not support Windows COFF format");
  }

  PatmosTargetELFStreamer *S = new PatmosTargetELFStreamer();
  return createELFStreamer(Ctx, S, MAB, _OS, _Emitter, RelaxAll, NoExecStack);
}

static MCStreamer * createPatmosMCAsmStreamer(MCContext &Ctx,
                            formatted_raw_ostream &OS,
                            bool isVerboseAsm, bool useLoc, bool useCFI,
                            bool useDwarfDirectory, MCInstPrinter *InstPrint,
                            MCCodeEmitter *CE, MCAsmBackend *TAB, bool ShowInst)
{
  PatmosTargetStreamer *S = new PatmosTargetAsmStreamer(OS);

  return llvm::createAsmStreamer(Ctx, S, OS, isVerboseAsm, useLoc, useCFI,
                                 useDwarfDirectory, InstPrint, CE, TAB,
                                 ShowInst);
}


static MCInstPrinter *createPatmosMCInstPrinter(const Target &T,
                                                unsigned SyntaxVariant,
                                                const MCAsmInfo &MAI,
						const MCInstrInfo &MII,
						const MCRegisterInfo &MRI,
                                                const MCSubtargetInfo &STI) {
  return new PatmosInstPrinter(MAI, MII, MRI);
}

extern "C" void LLVMInitializePatmosTargetMC() {
  // Register the MC asm info.
  TargetRegistry::RegisterMCAsmInfo(ThePatmosTarget, createPatmosMCAsmInfo);

  // Register the MC codegen info.
  TargetRegistry::RegisterMCCodeGenInfo(ThePatmosTarget,
                                        createPatmosMCCodeGenInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(ThePatmosTarget, createPatmosMCInstrInfo);

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
  TargetRegistry::RegisterAsmStreamer(ThePatmosTarget,
                                      createPatmosMCAsmStreamer);

  // Register the object streamer
  TargetRegistry::RegisterMCObjectStreamer(ThePatmosTarget,
                                           createPatmosMCStreamer);

}
