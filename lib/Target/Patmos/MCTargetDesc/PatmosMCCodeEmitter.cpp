//===-- PatmosMCCodeEmitter.cpp - Convert Patmos Code to Machine Code ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the PatmosMCCodeEmitter class.
//
//===----------------------------------------------------------------------===//
//
#define DEBUG_TYPE "mccodeemitter"
#include "MCTargetDesc/PatmosBaseInfo.h"
#include "MCTargetDesc/PatmosFixupKinds.h"
#include "MCTargetDesc/PatmosMCTargetDesc.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
class PatmosMCCodeEmitter : public MCCodeEmitter {
  PatmosMCCodeEmitter(const PatmosMCCodeEmitter &); // DO NOT IMPLEMENT
  void operator=(const PatmosMCCodeEmitter &); // DO NOT IMPLEMENT
  const MCInstrInfo &MCII;
  const MCSubtargetInfo &STI;
  MCContext &Ctx;

public:
  PatmosMCCodeEmitter(const MCInstrInfo &mcii, const MCSubtargetInfo &sti,
                    MCContext &ctx) :
            MCII(mcii), STI(sti) , Ctx(ctx) {}

  ~PatmosMCCodeEmitter() {}

  void EncodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups) const;


  // getBinaryCodeForInstr - TableGen'erated function for getting the
  // binary encoding for an instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups) const;


  /****** Callback functions for TableGen'ed getBinaryCodeForInstr ******/

   // getMachineOpValue - Return binary encoding of operand. If the machine
   // operand requires relocation, record the relocation and return zero.
  unsigned getMachineOpValue(const MCInst &MI,const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups) const;

  unsigned getPredOperandEncoding(const MCInst &MI, unsigned OpNo,
                                  SmallVectorImpl<MCFixup> &Fixups) const;

  /****** Helper functions to emit binary code ******/

  void EmitByte(unsigned char C, raw_ostream &OS) const {
    OS << (char)C;
  }

  void EmitInstruction(uint64_t Val, unsigned Size, raw_ostream &OS) const {
    // Output the instruction encoding in big endian byte order.
    for (unsigned i = 0; i < Size; ++i) {
      unsigned Shift = (Size - 1 - i) * 8;
      EmitByte((Val >> Shift) & 0xff, OS);
    }
  }

  /****** Helper functions to handle immediates and expressions ******/

  unsigned getImmediateEncoding(const MCInst &MI, const MCOperand &MO,
                           SmallVectorImpl<MCFixup> &Fixups) const;

  void
  addSymbolRefFixups(const MCInst &MI, const MCOperand& MO,
                                          const MCSymbolRefExpr* Expr,
                                          SmallVectorImpl<MCFixup> &Fixups) const;

}; // class PatmosMCCodeEmitter
}  // namespace

MCCodeEmitter *llvm::createPatmosMCCodeEmitter(const MCInstrInfo &MCII,
                                               const MCSubtargetInfo &STI,
                                               MCContext &Ctx)
{
  return new PatmosMCCodeEmitter(MCII, STI, Ctx);
}


/// EncodeInstruction - Emit the instruction.
/// Size the instruction (currently only 4 bytes
void PatmosMCCodeEmitter::
EncodeInstruction(const MCInst &MI, raw_ostream &OS,
                  SmallVectorImpl<MCFixup> &Fixups) const
{
  uint64_t Binary = getBinaryCodeForInstr(MI, Fixups);

  // Check for unimplemented opcodes.
  if (!Binary)
    llvm_unreachable("unimplemented opcode in EncodeInstruction()");

  const MCInstrDesc &Desc = MCII.get(MI.getOpcode());
  uint64_t TSFlags = Desc.TSFlags;

  // Pseudo instructions don't get encoded and shouldn't be here
  // in the first place!
  if ((TSFlags & PatmosII::FormMask) == PatmosII::FrmPseudo)
    llvm_unreachable("Pseudo opcode found in EncodeInstruction()");

  int Size = Desc.Size;

  // Encode Imm32 in ALUl
  if ((TSFlags & PatmosII::FormMask) == PatmosII::FrmALUl) {

    Binary <<= 32;

    const MCOperand& imm = MI.getOperand(MI.getNumOperands()-1);

    if (imm.isExpr()) {
      // TODO create fixup



    } else if (imm.isImm()) {
      Binary |= (0xFFFFFFFF & imm.getImm());
    } else {
      llvm_unreachable("Could not get immediate value of ALUl op.");
    }
  }

  if ((TSFlags & PatmosII::FormMask) != PatmosII::FrmALUl) {
    // TODO for other instructions, set bit 31 if they are the first one inside a bundle

  }


  EmitInstruction(Binary, Size, OS);
}

/// getMachineOpValue - Return binary encoding of operand. If the machine
/// operand requires relocation, record the relocation and return zero.
unsigned PatmosMCCodeEmitter::
getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                  SmallVectorImpl<MCFixup> &Fixups) const {
  if (MO.isReg()) {
    unsigned Reg = MO.getReg();
    unsigned RegNo = getPatmosRegisterNumbering(Reg);
    return RegNo;
  } else {
    return getImmediateEncoding(MI, MO, Fixups);
  }
}

/// getPredOperandEncoding - Return binary encoding of predicate operand.
unsigned
PatmosMCCodeEmitter::getPredOperandEncoding(const MCInst &MI, unsigned OpNo,
                                  SmallVectorImpl<MCFixup> &Fixups) const {
  // Base register is encoded in bits 20-16, offset is encoded in bits 15-0.
  assert( "Invalid predicate operand in encoder method!"
      && MI.getOperand(OpNo).isReg() && MI.getOperand(OpNo+1).isImm() );
  unsigned RegBits = getMachineOpValue(MI, MI.getOperand(OpNo),   Fixups);
  unsigned InvBit  = getMachineOpValue(MI, MI.getOperand(OpNo+1), Fixups);

  return (InvBit << 3) | RegBits;
}

unsigned
PatmosMCCodeEmitter::getImmediateEncoding(const MCInst &MI, const MCOperand& MO,
                                  SmallVectorImpl<MCFixup> &Fixups) const {
  if (MO.isImm()) {
    return static_cast<unsigned>(MO.getImm());
  } else if (MO.isFPImm()) {
    return static_cast<unsigned>(APFloat(MO.getFPImm())
        .bitcastToAPInt().getHiBits(32).getLimitedValue());
  }

  // MO must be an Expr.
  assert(MO.isExpr());

  const MCExpr *Expr = MO.getExpr();
  MCExpr::ExprKind Kind = Expr->getKind();

  if (Kind == MCExpr::Binary) {
    Expr = static_cast<const MCBinaryExpr*>(Expr)->getLHS();
    Kind = Expr->getKind();
  }

  assert (Kind == MCExpr::SymbolRef);

  addSymbolRefFixups(MI, MO, cast<MCSymbolRefExpr>(Expr), Fixups);

  // All of the information is in the fixup.
  return 0;
}

void
PatmosMCCodeEmitter::addSymbolRefFixups(const MCInst &MI, const MCOperand& MO,
                                        const MCSymbolRefExpr* Expr,
                                        SmallVectorImpl<MCFixup> &Fixups) const
{

  // TODO handle fixups properly

  Patmos::Fixups FixupKind = Patmos::Fixups(FK_Data_2);

  /*
  switch(Expr->getKind()) {
  default:
    break;
  case MCSymbolRefExpr::VK_None:
    break;
  } // switch

  switch (MI.getOpcode()) {
  default:
    FixupKind = FK_Data_2;
    Fixups.push_back(MCFixup::Create(0, Expr, FixupKind));
    break;
  }
  */


  Fixups.push_back(MCFixup::Create(0, MO.getExpr(), MCFixupKind(FixupKind)));
}

#include "PatmosGenMCCodeEmitter.inc"

