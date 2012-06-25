//===-- PatmosMCInstLower.cpp - Convert Patmos MachineInstr to an MCInst---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower Patmos MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "PatmosMCInstLower.h"
#include "MCTargetDesc/PatmosBaseInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Target/Mangler.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/SmallString.h"

using namespace llvm;


MCOperand PatmosMCInstLower::LowerSymbolOperand(const MachineOperand &MO, unsigned Offset) const {

  MCSymbolRefExpr::VariantKind Kind;
  const MCSymbol *Symbol;

  // TODO support CREL only for MBBs?

  switch (MO.getTargetFlags()) {
  case PatmosII::MO_NO_FLAG:    Kind = MCSymbolRefExpr::VK_None; break;
  case PatmosII::MO_CREL:       Kind = MCSymbolRefExpr::VK_Patmos_CREL; break;
  default: llvm_unreachable("Unknown target flag on GV operand");
  }

  switch (MO.getType()) {
  case MachineOperand::MO_MachineBasicBlock:
    Symbol = MO.getMBB()->getSymbol();
    // symbols to BBs are always cache relative (?)
    // TODO set this earlier so we do not need it here, then remove it.
    Kind = MCSymbolRefExpr::VK_Patmos_CREL;
    break;
  case MachineOperand::MO_GlobalAddress:
    Symbol = Printer.Mang->getSymbol(MO.getGlobal());
    break;
  case MachineOperand::MO_BlockAddress:
    Symbol = Printer.GetBlockAddressSymbol(MO.getBlockAddress());
    break;
  case MachineOperand::MO_ExternalSymbol:
    Symbol = Printer.GetExternalSymbolSymbol(MO.getSymbolName());
    break;
  case MachineOperand::MO_JumpTableIndex:
    Symbol = Printer.GetJTISymbol(MO.getIndex());
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    Symbol = Printer.GetCPISymbol(MO.getIndex());
    if (MO.getOffset())
      Offset += MO.getOffset();
    break;

  default: llvm_unreachable("unknown symbol operand type");
  }

  const MCSymbolRefExpr *MCSym = MCSymbolRefExpr::Create(Symbol, Kind, Ctx);

  if (!Offset)
    return MCOperand::CreateExpr(MCSym);

  // Assume offset is never negative.
  assert(Offset > 0);

  const MCConstantExpr *OffsetExpr =  MCConstantExpr::Create(Offset, Ctx);
  const MCBinaryExpr *AddExpr = MCBinaryExpr::CreateAdd(MCSym, OffsetExpr, Ctx);
  return MCOperand::CreateExpr(AddExpr);
}


MCOperand PatmosMCInstLower::LowerOperand(const MachineOperand &MO, unsigned Offset) const {

  switch (MO.getType()) {
  default:
    llvm_unreachable("unknown operand type");
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands.
    if (MO.isImplicit()) break;
    return MCOperand::CreateReg(MO.getReg());
  case MachineOperand::MO_Immediate:
    return MCOperand::CreateImm(MO.getImm() + Offset);
  case MachineOperand::MO_MachineBasicBlock:
  case MachineOperand::MO_GlobalAddress:
  case MachineOperand::MO_ExternalSymbol:
  case MachineOperand::MO_JumpTableIndex:
  case MachineOperand::MO_ConstantPoolIndex:
  case MachineOperand::MO_BlockAddress:
    return LowerSymbolOperand(MO, Offset);
  case MachineOperand::MO_Metadata:
    // TODO handle Metadata somehow??
  case MachineOperand::MO_RegisterMask:
    break;
  }

  return MCOperand();
}


void PatmosMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
  OutMI.setOpcode(MI->getOpcode());

  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);

    MCOperand MCOp = LowerOperand(MO);
    if (MCOp.isValid()) {
      OutMI.addOperand(MCOp);
    }
  }
}

