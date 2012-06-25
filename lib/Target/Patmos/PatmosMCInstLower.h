//===-- PatmosMCInstLower.h - Lower MachineInstr to MCInst ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef _PATMOS_MCINSTLOWER_H_
#define _PATMOS_MCINSTLOWER_H_

#include "llvm/Support/Compiler.h"

namespace llvm {
  class AsmPrinter;
  class MCAsmInfo;
  class MCContext;
  class MCInst;
  class MCOperand;
  class MCSymbol;
  class MachineInstr;
  class MachineModuleInfoMachO;
  class MachineOperand;
  class Mangler;

  /// PatmosMCInstLower - This class is used to lower an MachineInstr
  /// into an MCInst.
class LLVM_LIBRARY_VISIBILITY PatmosMCInstLower {
  MCContext &Ctx;

  AsmPrinter &Printer;
public:
  PatmosMCInstLower(MCContext &ctx, AsmPrinter &printer)
    : Ctx(ctx), Printer(printer) {}

  MCContext &getContext() { return Ctx; }

  void Lower(const MachineInstr *MI, MCInst &OutMI) const;

  MCOperand LowerOperand(const MachineOperand &MO, unsigned Offset = 0) const;

  MCOperand LowerSymbolOperand(const MachineOperand &MO, unsigned Offset = 0) const;
};

}

#endif // _PATMOS_MCINSTLOWER_H_
