//===-- PatmosBaseInfo.h - Top level definitions for PATMOS MC ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone helper functions and enum definitions for
// the Patmos target useful for the compiler back-end and the MC libraries.
//
//===----------------------------------------------------------------------===//
#ifndef PATMOSBASEINFO_H
#define PATMOSBASEINFO_H

#include "PatmosFixupKinds.h"
#include "PatmosMCTargetDesc.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {

/// PatmosII - This namespace holds all of the target specific flags that
/// instruction info tracks.
///
namespace PatmosII {

  enum {
    //===------------------------------------------------------------------===//
    // Instruction encodings.  These are the standard/most common forms for
    // Patmos instructions. This must be consistent with PatmosInstrFormats.td.
    //

    /// FrmOther - This form is for instructions that have no specific format.
    FrmOther    = 0,

    // Pseudo - This represents an instruction that is a pseudo instruction
    // or one that has not been implemented yet.  It is illegal to code generate
    // it, but tolerated for intermediate implementation stages.
    // Note that this does not apply to 'pseudo' instruction aliases like 'mov'
    // which are translated to a normal instruction.
    FrmPseudo   = 1,

    /// FrmALUi - This format is for instructions of the ALUi format (12bit immediate).
    FrmALUi     = 2,

    /// FrmALUl - This format is for instructions of the ALUl format (32bit immediate).
    FrmALUl     = 3,

    /// FrmALUc - This format is for instructions of the ALUc format (Pd = Rs1 op Rs2).
    FrmALUc     = 4,

    /// FrmALUp - This format is for instructions of the ALUp format (Pd = Ps1 op Ps2).
    FrmALUp     = 5,

    /// FrmMem - This format is for memory instructions with 7bit offset.
    FrmMem      = 6,

    /// FrmSTC - This form is for instructions of the STC format (stack control, 22bit immediate).
    FrmSTC      = 7,

    /// FrmPFLb - This form is for instructions of the PBLb format (flow control, 22bit immediate).
    FrmPFLb     = 8,

    FormMask    = 0x0F
  };

}

/// getPatmosPredicateIndex - Given an instruction description, return the
/// index of the predicate operand. Check MCID.isPredicable to see if the instruction
/// actually has a predicate.
inline static unsigned getPatmosPredicateIndex(const MCInstrDesc &MCID) {
  // This must be consistent with PatmosInstrFormats.td
  return (MCID.TSFlags & 0xF0) >> 4;
}

/// getPatmosRegisterNumbering - Given the enum value for some register,
/// return the number that it corresponds to (the binary representation).
inline static unsigned getPatmosRegisterNumbering(unsigned RegEnum)
{
  // TODO get this from tblgen somehow? (from PatmosReg.Num)

  using namespace Patmos;
  switch (RegEnum) {
  case NoRegister: return 0; // Required for default P0 guard
  case R0:  case SZ:  case P0:  return 0;
  case R1:  case SM:  case P1:  return 1;
  case R2:  case SL:  case P2:  return 2;
  case R3:  case SH:  case P3:  return 3;
  case R4:  case SB:  case P4:  return 4;
  case R5:  case SO:  case P5:  return 5;
  case R6:  case ST:  case P6:  return 6;
  case R7:  case S7:  case P7:  return 7;
  case R8:  case S8:  return 8;
  case R9:  case S9:  return 9;
  case R10: case S10: return 10;
  case R11: case S11: return 11;
  case R12: case S12: return 12;
  case R13: case S13: return 13;
  case R14: case S14: return 14;
  case R15: case S15: return 15;

  case R16: return 16;
  case R17: return 17;
  case R18: return 18;
  case R19: return 19;
  case R20: return 20;
  case R21: return 21;
  case R22: return 22;
  case R23: return 23;
  case R24: return 24;
  case R25: return 25;
  case R26: return 26;
  case R27: return 27;
  case R28: return 28;
  case R29: return 29;
  case RFP: return 30;
  case RSP: return 31;
  default:
    llvm_unreachable("Unknown Patmos register!");
  }
}

}

#endif
