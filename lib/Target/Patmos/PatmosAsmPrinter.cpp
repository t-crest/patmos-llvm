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


    /// isBlockOnlyReachableByFallthough - Return true if the basic block has
    /// exactly one predecessor and the control transfer mechanism between
    /// the predecessor and this block is a fall-through.
    ///
    /// This overrides AsmPrinter's implementation to handle delay slots.
    virtual bool isBlockOnlyReachableByFallthrough(const MachineBasicBlock *MBB) const;

    //===------------------------------------------------------------------===//
    // Inline Asm Support
    //===------------------------------------------------------------------===//

    virtual bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                 unsigned AsmVariant, const char *ExtraCode,
                                 raw_ostream &OS);

    virtual bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                                       unsigned AsmVariant,
                                       const char *ExtraCode,
                                       raw_ostream &OS);
  };
} // end of anonymous namespace

void PatmosAsmPrinter::EmitInstruction(const MachineInstr *MI) {
  MCInst TmpInst;
  MCInstLowering.Lower(MI, TmpInst);

  // TODO Handle bundles (mark the first instruction after a bundle marker as bundled)
  bool isBundled = false;

  TmpInst.addOperand(MCOperand::CreateImm(isBundled));

  OutStreamer.EmitInstruction(TmpInst);
}


bool PatmosAsmPrinter::
isBlockOnlyReachableByFallthrough(const MachineBasicBlock *MBB) const {
  // If this is a landing pad, it isn't a fall through.  If it has no preds,
  // then nothing falls through to it.
  if (MBB->isLandingPad() || MBB->pred_empty())
    return false;

  // If there isn't exactly one predecessor, it can't be a fall through.
  MachineBasicBlock::const_pred_iterator PI = MBB->pred_begin(), PI2 = PI;
  if (++PI2 != MBB->pred_end())
    return false;

  // The predecessor has to be immediately before this block.
  const MachineBasicBlock *Pred = *PI;

  if (!Pred->isLayoutSuccessor(MBB))
    return false;

  // If the block is completely empty, then it definitely does fall through.
  if (Pred->empty())
    return true;


  // Here is the difference to the AsmPrinter method;
  // We do not check properties of all terminator instructions
  // (delay slot instructions do not have to be terminators),
  // but instead check if the *last terminator* is an
  // unconditional branch (no barrier)
  MachineBasicBlock::const_iterator I = Pred->end();
  // find last terminator
  while (I != Pred->begin() && !(--I)->isTerminator()) ;
  return I == Pred->end() || !I->isBarrier();
}


bool PatmosAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                 unsigned AsmVariant, const char *ExtraCode,
                                 raw_ostream &O)
{


  return false;
}

bool PatmosAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                                       unsigned AsmVariant,
                                       const char *ExtraCode, raw_ostream &O) {
  // Target doesn't support this yet!
  return true;
}



////////////////////////////////////////////////////////////////////////////////

// Force static initialization.
extern "C" void LLVMInitializePatmosAsmPrinter() {
  RegisterAsmPrinter<PatmosAsmPrinter> X(ThePatmosTarget);
}
