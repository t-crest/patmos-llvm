//===-- PatmosInstPrinter.cpp - Convert Patmos MCInst to assembly syntax --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints an Patmos MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "asm-printer"
#include "Patmos.h"
#include "PatmosInstPrinter.h"
#include "MCTargetDesc/PatmosBaseInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
using namespace llvm;


// Include the auto-generated portion of the assembly writer.
#include "PatmosGenAsmWriter.inc"

void PatmosInstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                                  StringRef Annot) {
  printInstruction(MI, O);
  printAnnotation(O, Annot);
}

void PatmosInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                     raw_ostream &O, const char *Modifier) {
  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    // do not print register R0 in addressing modes
    if ((Modifier && strcmp(Modifier, "addrmod") != 0) ||
        (Op.getReg() != Patmos::R0))
      O << getRegisterName(Op.getReg());
  } else if (Op.isImm()) {
    if (Modifier && strcmp(Modifier, "addrmod") == 0) {
      const MCOperand &baseOp = MI->getOperand(OpNo - 1);
      if (baseOp.getReg() == Patmos::R0)
        O << Op.getImm();
      else if (Op.getImm() != 0)
        O << ((Op.getImm() < 0) ? " - " : " + ") << std::abs(Op.getImm());
    }
    else
      O << Op.getImm();
  } else {
    assert(Op.isExpr() && "unknown operand kind in printOperand");

    if (Modifier && strcmp(Modifier, "addrmod") == 0)
      O << " + ";

    O << *Op.getExpr();
  }
}

void PatmosInstPrinter::printPredicateOperand(const MCInst *MI, unsigned OpNo, raw_ostream &O) {
  unsigned reg  = MI->getOperand(OpNo  ).getReg();
  int      flag = MI->getOperand(OpNo+1).getImm();

  const MCInstrDesc &Desc = MII.get(MI->getOpcode());
  uint64_t TSFlags = Desc.TSFlags;

  // handles predicate source operands for predicate combine instructions
  if ((TSFlags & PatmosII::FormMask) == PatmosII::FrmALUp && OpNo > 2) {
    O << ((flag)?"!":" ") << getRegisterName(reg);
    return;
  }

  if (reg == Patmos::NoRegister || ((reg == Patmos::P0) && !flag)) {
    O << "     "; // instead of ( p0)
  } else {
    O << "(" << ((flag)?"!":" ") << getRegisterName(reg) << ")";
  }
}
