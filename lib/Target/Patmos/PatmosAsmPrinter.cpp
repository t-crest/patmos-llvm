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
#include "PatmosAsmPrinter.h"
#include "PatmosMCInstLower.h"
#include "PatmosTargetMachine.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosUtil.h"
#include "InstPrinter/PatmosInstPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCContext.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;


/// EnableBasicBlockSymbols - If enabled, symbols for basic blocks are emitted
/// containing the name of their IR representation, appended by their
/// MBB number (for uniqueness). Note that this also includes MBBs which are
/// not necessarily branch/call targets.
static cl::opt<bool> EnableBasicBlockSymbols(
  "mpatmos-enable-bb-symbols",
  cl::init(false),
  cl::desc("Enable (additional) generation of symbols "
           "named like the IR basic blocks."),
  cl::Hidden);



void PatmosAsmPrinter::EmitFunctionEntryLabel() {
  // Create a temp label that will be emitted at the end of the first cache block (at the end of the function
  // if the function has only one cache block)
  CurrCodeEnd = OutContext.CreateTempSymbol();

  // TODO the alignment of the machinefunction and basic blocks is never set.
  //      either use the TargetMachine I-cache alignment info as minimum value
  //      or set alignments in some separate pass (must be after the function
  //      splitter, or both in function splitter and a separate pass)!

  // convert LLVM's log2 function alignment
  unsigned alignment = std::max(4u, 1u << MF->getAlignment());

  // emit a function/subfunction start directive
  EmitFStart(CurrentFnSymForSize, CurrCodeEnd, alignment);

  // Now emit the normal function label
  AsmPrinter::EmitFunctionEntryLabel();
}


void PatmosAsmPrinter::EmitBasicBlockBegin(const MachineBasicBlock *MBB) {
  // If special generation of BB symbols is enabled,
  // do so for every MBB.
  if (EnableBasicBlockSymbols) {
    // create a symbol with the BBs name
    SmallString<128> bbname;
    getMBBIRName(MBB, bbname);
    MCSymbol *bbsym = OutContext.GetOrCreateSymbol(bbname.str());
    OutStreamer.EmitLabel(bbsym);

    // set basic block size
    unsigned int bbsize = 0;
    for(MachineBasicBlock::const_instr_iterator i(MBB->instr_begin()),
        ie(MBB->instr_end()); i!=ie; ++i)
    {
      bbsize += i->getDesc().Size;
    }
    OutStreamer.EmitELFSize(bbsym, MCConstantExpr::Create(bbsize, OutContext));
  }
}


void PatmosAsmPrinter::EmitBasicBlockEnd(const MachineBasicBlock *MBB) {
  // EmitBasicBlockBegin emits after the label, too late for emitting .fstart,
  // so we do it at the end of the previous block of a cache block start MBB.
  if (&MBB->getParent()->back() == MBB) return;
  const MachineBasicBlock *Next = MBB->getNextNode();

  // skip blocks that are in the same cache block..
  if (!isFStart(Next)) return;

  // Next is the start of a new cache block, close the old one and start a new cache block
  OutStreamer.EmitLabel(CurrCodeEnd);

  // We need an address symbol from the next block
  assert(!Next->pred_empty() && "Basic block without predecessors do not emit labels, unsupported.");

  MCSymbol *SymStart = Next->getSymbol();

  // create new end symbol
  CurrCodeEnd = OutContext.CreateTempSymbol();

  // mark the symbol as method-cache-cacheable code
  OutStreamer.EmitSymbolAttribute(SymStart, MCSA_ELF_TypeCode);

  // emit a .size directive
  EmitDotSize(SymStart, CurrCodeEnd);

  // convert LLVM's log2-block alignment to bytes
  unsigned alignment = std::max(4u, 1u << Next->getAlignment());

  // emit a function/subfunction start directive
  EmitFStart(SymStart, CurrCodeEnd, alignment);
}

void PatmosAsmPrinter::EmitFunctionBodyEnd() {
  // Emit the end symbol of the last cache block
  OutStreamer.EmitLabel(CurrCodeEnd);
}

void PatmosAsmPrinter::EmitDotSize(MCSymbol *SymStart, MCSymbol *SymEnd) {
  const MCExpr *SizeExpr =
    MCBinaryExpr::CreateSub(MCSymbolRefExpr::Create(SymEnd,   OutContext),
                            MCSymbolRefExpr::Create(SymStart, OutContext),
                            OutContext);

  OutStreamer.EmitELFSize(SymStart, SizeExpr);
}

void PatmosAsmPrinter::EmitFStart(MCSymbol *SymStart, MCSymbol *SymEnd,
                                     unsigned Alignment) {
  // emit .fstart SymStart, SymEnd-SymStart
  const MCExpr *SizeExpr =
    MCBinaryExpr::CreateSub(MCSymbolRefExpr::Create(SymEnd,   OutContext),
                            MCSymbolRefExpr::Create(SymStart, OutContext),
                            OutContext);

  OutStreamer.EmitFStart(SymStart, SizeExpr, Alignment);
}

bool PatmosAsmPrinter::isFStart(const MachineBasicBlock *MBB) const {
  // query the machineinfo object - the PatmosFunctionSplitter, or some other
  // pass, has marked all entry blocks already.
  const PatmosMachineFunctionInfo *PMFI =
                                       MF->getInfo<PatmosMachineFunctionInfo>();
  return PMFI->isMethodCacheRegionEntry(MBB);
}


void PatmosAsmPrinter::EmitInstruction(const MachineInstr *MI) {

  SmallVector<const MachineInstr*, 2> BundleMIs;
  unsigned Size = 1;

  // Unpack BUNDLE instructions
  if (MI->isBundle()) {

    const MachineBasicBlock *MBB = MI->getParent();
    MachineBasicBlock::const_instr_iterator MII = MI;
    ++MII;
    unsigned int IgnoreCount = 0;
    while (MII != MBB->end() && MII->isInsideBundle()) {
      const MachineInstr *MInst = MII;
      if (MInst->getOpcode() == TargetOpcode::DBG_VALUE ||
          MInst->getOpcode() == TargetOpcode::IMPLICIT_DEF) {
          IgnoreCount++;
          ++MII;
          continue;
      }
      BundleMIs.push_back(MInst);
      ++MII;
    }
    Size = BundleMIs.size();
    assert((Size+IgnoreCount) == MI->getBundleSize() && "Corrupt Bundle!");
  }
  else {
    BundleMIs.push_back(MI);
  }

  // Emit all instructions in the bundle.
  for (unsigned Index = 0; Index < Size; Index++) {
    MCInst MCI;
    MCInstLowering.Lower(BundleMIs[Index], MCI);

    // Set bundle marker
    bool isBundled = (Index < Size - 1);
    MCI.addOperand(MCOperand::CreateImm(isBundled));

    OutStreamer.EmitInstruction(MCI);
  }
}


bool PatmosAsmPrinter::
isBlockOnlyReachableByFallthrough(const MachineBasicBlock *MBB) const {
  // If this is a landing pad, it isn't a fall through.  If it has no preds,
  // then nothing falls through to it.
  if (MBB->isLandingPad() || MBB->pred_empty() || MBB->hasAddressTaken())
    return false;

  // If there isn't exactly one predecessor, it can't be a fall through.
  MachineBasicBlock::const_pred_iterator PI = MBB->pred_begin(), PI2 = PI;
  if (++PI2 != MBB->pred_end())
    return false;

  // The predecessor has to be immediately before this block.
  const MachineBasicBlock *Pred = *PI;

  if (!Pred->isLayoutSuccessor(MBB))
    return false;

  // if the block starts a new cache block, do not fall through (we need to
  // insert cache stuff, even if we only reach this block from a jump from the
  // previous block, and we need the label).
  if (isFStart(MBB))
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
  // inline-assembly. No need for pretty formatting of default ops, output is
  // for AsmParser only.

  // TODO any special handling of predicates (flags) or anything?

  MCInst MCI;
  MCInstLowering.Lower(MI, MCI);

  PatmosInstPrinter PIP(OutContext.getAsmInfo(), *TM.getInstrInfo(),
                        *TM.getRegisterInfo());

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
