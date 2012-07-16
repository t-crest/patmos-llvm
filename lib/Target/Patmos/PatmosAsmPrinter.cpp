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
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCContext.h"
#include "llvm/Support/TargetRegistry.h"
#include "InstPrinter/PatmosInstPrinter.h"

using namespace llvm;

namespace {
  class PatmosAsmPrinter : public AsmPrinter {
  private:
    PatmosMCInstLower MCInstLowering;

    // symbol to use for the end of the currently emitted cache block
    MCSymbol *CurrFRELEnd;

  public:
    PatmosAsmPrinter(TargetMachine &TM, MCStreamer &Streamer)
      : AsmPrinter(TM, Streamer), MCInstLowering(OutContext, *this), CurrFRELEnd(0) {}

    virtual const char *getPassName() const {
      return "Patmos Assembly Printer";
    }

    virtual void EmitFunctionEntryLabel();

    virtual void EmitBasicBlockEnd(const MachineBasicBlock *);

    virtual void EmitFunctionBodyEnd();

    // called in the framework for instruction printing
    virtual void EmitInstruction(const MachineInstr *MI);


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
  private:
    void EmitFRELStart(MCSymbol *SymStart, MCSymbol *SymEnd, unsigned Alignment = 0);

    bool isFRELStart(const MachineBasicBlock *MBB) const;
  };

} // end of anonymous namespace


void PatmosAsmPrinter::EmitFunctionEntryLabel() {
  // Create a temp label that will be emitted at the end of the first cache block (at the end of the function
  // if the function has only one cache block)
  CurrFRELEnd = OutContext.CreateTempSymbol();

  // emit a function/subfunction start directive
  EmitFRELStart(CurrentFnSymForSize, CurrFRELEnd);

  // Now emit the normal function label
  AsmPrinter::EmitFunctionEntryLabel();
}

void PatmosAsmPrinter::EmitBasicBlockEnd(const MachineBasicBlock *MBB) {
  // EmitBasicBlockBegin emits after the label, too late for emitting FREL stuff,
  // so we do it at the end of the previous block of a cache block start MBB.
  if (&MBB->getParent()->back() == MBB) return;
  const MachineBasicBlock *Next = MBB->getNextNode();

  // skip blocks that are in the same cache block..
  if (!isFRELStart(Next)) return;

  // Next is the start of a new cache block, close the old one and start a new cache block
  OutStreamer.EmitLabel(CurrFRELEnd);

  // We need an address symbol from the next block
  assert(!MBB->pred_empty() && "Basic block without predecessors do not emit labels, unsupported.");

  MCSymbol *SymStart = MBB->getSymbol();

  // create new end symbol
  CurrFRELEnd = OutContext.CreateTempSymbol();

  // emit a function/subfunction start directive
  EmitFRELStart(SymStart, CurrFRELEnd, Next->getAlignment());
}

void PatmosAsmPrinter::EmitFunctionBodyEnd() {
  // Emit the end symbol of the last cache block
  OutStreamer.EmitLabel(CurrFRELEnd);
}

void PatmosAsmPrinter::EmitFRELStart(MCSymbol *SymStart, MCSymbol *SymEnd, unsigned Alignment) {
  // emit .frelstart SymStart, SymEnd-SymStart
  const MCExpr *SizeExpr =
    MCBinaryExpr::CreateSub(MCSymbolRefExpr::Create(SymEnd,   OutContext),
                            MCSymbolRefExpr::Create(SymStart, OutContext),
                            OutContext);

  // TODO get the proper cache alignment size from the Patmos configuration somehow, merge with per-block alignment
  Alignment = Alignment != 0 ? Alignment : 16;

  // TODO should we emit a .type directive for SymStart instead of setting the flag in EmitFREL?
  // This would need adding a new MCSymbolAttr, and adding proper handling in EmitSymbolAttribute and
  // in the parser. Similarly, should we emit .size or handle this in EmitFREL as well?
  // - If we emit .size, we should also emit .type for consistency reasons
  // - If we emit .type, we need to add the @fblock type to ELF headers, emitters and parser.
  // - If we emit .size as part of .fstart, we need a separate symbol for function start and cache-start!
  //   (otherwise, we emit two .size directives for the function label, one with function size, one with the
  //   size of the first cache block!)
  // - If we emit .size, we also need to make sure the linker does not interpret it as offset.
  //
  // For now, we set the SubFunction flag in EmitFRELStart, and do not set size.

  //OutStreamer.EmitSymbolAttribute(SymStart, MCSA_ELF_TypeSubFunction);

  //OutStreamer.EmitELFSize(SymStart, SizeExpr);

  OutStreamer.EmitFRELStart(SymStart, SizeExpr, Alignment);
}

bool PatmosAsmPrinter::isFRELStart(const MachineBasicBlock *MBB) const {
  // TODO this is just a temporary hack, need to define some custom attribute for marking
  // blocks as start of a cache block
  return (MBB->getAlignment() != 0);
}


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

  // if the block starts a new cache block, do not fall through (we need to insert cache stuff, even
  // if we only reach this block from a jump from the previous block, and we need the label).
  if (isFRELStart(MBB))
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
  // Does this asm operand have a single letter operand modifier?
  if (ExtraCode && ExtraCode[0])
    return true; // Unknown modifier.

  // Print operand for inline-assembler. Basically the same code as in
  // PatmosInstPrinter::printOperand, but for MachineOperand and for
  // inline-assembly. No need for pretty formatting of default ops, output is for
  // AsmParser only.

  // TODO any special handling of predicates (flags) or anything?

  MCInst MCI;
  MCInstLowering.Lower(MI, MCI);

  PatmosInstPrinter PIP(OutContext.getAsmInfo(), *TM.getInstrInfo(), *TM.getRegisterInfo(), true);

  PIP.printOperand(&MCI, OpNo, O, ExtraCode);

  return false;
}

bool PatmosAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                                       unsigned AsmVariant,
                                       const char *ExtraCode, raw_ostream &O)
{
  if (ExtraCode && ExtraCode[0])
     return true; // Unknown modifier.

  const MachineOperand &MO = MI->getOperand(OpNo);
  assert(MO.isReg() && "unexpected inline asm memory operand");
  O << "[$" << PatmosInstPrinter::getRegisterName(MO.getReg()) << "]";
  return false;
}



////////////////////////////////////////////////////////////////////////////////

// Force static initialization.
extern "C" void LLVMInitializePatmosAsmPrinter() {
  RegisterAsmPrinter<PatmosAsmPrinter> X(ThePatmosTarget);
}
