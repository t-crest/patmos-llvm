//===-- PatmosAsmPrinter.cpp - Patmos LLVM assembly writer ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is here from the legacy asm printing infrastructure and
// probably will vanish one day.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "asm-printer"
#include "PatmosMCInstLower.h"
#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace {
  class PatmosAsmPrinter : public AsmPrinter {
  private:
    PatmosMCInstLower MCInstLowering;
  public:
    PatmosAsmPrinter(TargetMachine &TM, MCStreamer &Streamer)
      : AsmPrinter(TM, Streamer), MCInstLowering(OutContext, *Mang, *this) {}

    virtual const char *getPassName() const {
      return "Patmos Assembly Printer";
    }

    // called in the framework for instruction printing
    void EmitInstruction(const MachineInstr *MI);
  };
} // end of anonymous namespace

void PatmosAsmPrinter::EmitInstruction(const MachineInstr *MI) {
  MCInst TmpInst;
  MCInstLowering.Lower(MI, TmpInst);
  OutStreamer.EmitInstruction(TmpInst);
}

// Force static initialization.
extern "C" void LLVMInitializePatmosAsmPrinter() {
  RegisterAsmPrinter<PatmosAsmPrinter> X(ThePatmosTarget);
}
