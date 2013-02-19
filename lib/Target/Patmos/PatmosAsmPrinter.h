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

#ifndef _LLVM_TARGET_PATMOS_ASMPRINTER_H_
#define _LLVM_TARGET_PATMOS_ASMPRINTER_H_

#include "PatmosTargetMachine.h"
#include "PatmosMCInstLower.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCContext.h"

namespace llvm {
  class PatmosAsmPrinter : public AsmPrinter {
  private:
    PatmosTargetMachine *PTM;

    PatmosMCInstLower MCInstLowering;

    // symbol to use for the end of the currently emitted subfunction
    MCSymbol *CurrCodeEnd;

  public:
    PatmosAsmPrinter(TargetMachine &TM, MCStreamer &Streamer)
      : AsmPrinter(TM, Streamer), MCInstLowering(OutContext, *this), CurrCodeEnd(0)
    {
      if (!(PTM = static_cast<PatmosTargetMachine*>(&TM))) {
        llvm_unreachable("PatmosAsmPrinter must be initialized with a Patmos target configuration.");
      }
      PTM->setMCSaveTempLabels(true);
    }

    virtual const char *getPassName() const {
      return "Patmos Assembly Printer";
    }

    virtual void EmitFunctionEntryLabel();

    virtual void EmitBasicBlockBegin(const MachineBasicBlock *MBB);
    virtual void EmitBasicBlockEnd(const MachineBasicBlock *);

    virtual void EmitFunctionBodyEnd();

    // called in the framework for instruction printing
    virtual void EmitInstruction(const MachineInstr *MI);

    /// EmitDotSize - Emit a .size directive using SymEnd - SymStart.
    void EmitDotSize(MCSymbol *SymStart, MCSymbol *SymEnd);

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
    /// mark the start of an subfunction relocation area.
    /// Alignment is in bytes.
    void EmitFStart(MCSymbol *SymStart, MCSymbol *SymEnd,
                       unsigned Alignment = 0);

    bool isFStart(const MachineBasicBlock *MBB) const;
  };

} // end of llvm namespace

#endif
