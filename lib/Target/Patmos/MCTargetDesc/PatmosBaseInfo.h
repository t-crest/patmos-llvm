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

  enum TOF {
    //===------------------------------------------------------------------===//
    // Patmos Specific MachineOperand flags.

    MO_NO_FLAG

  };

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

    /// FrmALUb - This format is for instructions of the ALUb format (bitcopy).
    FrmALUb     = 5,

    /// FrmALUci - This format is for instructions of the ALUci format (Pd = Rs1 op imm).
    FrmALUci    = 6,

    /// FrmALUp - This format is for instructions of the ALUp format (Pd = Ps1 op Ps2).
    FrmALUp     = 7,

    /// FrmLDT - This format is for memory load instructions with 7bit offset.
    FrmLDT      = 8,

    /// FrmSTT - This format is for memory store instructions with 7bit offset.
    FrmSTT      = 9,

    /// FrmSTCi - This form is for instructions of the STC format (stack control, 22bit immediate).
    FrmSTCi     = 10,

    /// FrmSTCr - This form is for instructions of the STC format (stack control, register).
    FrmSTCr     = 11,

    /// FrmCFLi - This form is for instructions of the CFLi format (flow control, 22bit immediate).
    FrmCFLi     = 12,

    /// FrmCFLri - This form is for instructions of the CFLri format (flow control, implicit registers).
    FrmCFLri    = 13,
    /// FrmCFLrs - This form is for instructions of the CFLrs format (flow control, single register).
    FrmCFLrs    = 14,
    /// FrmCFLrt - This form is for instructions of the CFLrt format (flow control, two registers).
    FrmCFLrt    = 15,

    FormMask    = 0x0F
  };

  enum MemType {
    //===------------------------------------------------------------------===//
    // Patmos Memory Types.
    MEM_S = 0, // stack cache
    MEM_L = 1, // local memory (scratchpad)
    MEM_C = 2, // data cache
    MEM_M = 3  // main memory (bypass caches)
  };
}

inline static unsigned getPatmosFormat(uint64_t TSFlags) {
  return (TSFlags & PatmosII::FormMask);
}

inline static unsigned getPatmosImmediateOpNo(uint64_t TSFlags) {
  return (TSFlags >> 4) & 0x0F;
}

inline static unsigned getPatmosImmediateShift(uint64_t TSFlags) {
  return (TSFlags >> 8) & 0x07;
}

inline static bool isPatmosImmediateSigned(uint64_t TSFlags) {
  return (TSFlags >> 11) & 0x01;
}

inline static bool isPatmosMayStall(uint64_t TSFlags) {
  return (TSFlags >> 12) & 0x01;
}

inline static bool hasPatmosImmediate(uint64_t TSFlags) {
  // We assume that the first operand is always the predicate register
  return getPatmosImmediateOpNo(TSFlags) > 0;
}

inline static bool isPatmosCFL(unsigned opcode, uint64_t TSFlags) {
  switch (TSFlags & PatmosII::FormMask) {
  case PatmosII::FrmCFLi:
  case PatmosII::FrmCFLri:
  case PatmosII::FrmCFLrs:
  case PatmosII::FrmCFLrt:
    return true;
  }
  return false;
}

inline static unsigned getPatmosImmediateSize(uint64_t TSFlags) {
  switch (TSFlags & PatmosII::FormMask) {
  case PatmosII::FrmALUb:  return 5;
  case PatmosII::FrmALUci: return 5;
  case PatmosII::FrmLDT:   return 7;
  case PatmosII::FrmSTT:   return 7;
  case PatmosII::FrmALUi:  return 12;
  case PatmosII::FrmSTCi:  return 22;
  case PatmosII::FrmCFLi:  return 22;
  case PatmosII::FrmALUl:  return 32;
  }
  return 0;
}

/// getPatmosRegisterNumbering - Given the enum value for some register,
/// return the number that it corresponds to (the binary representation).
inline static unsigned getPatmosRegisterNumbering(unsigned RegEnum)
{
  // TODO get this from tblgen somehow? (from PatmosReg.Num)

  using namespace Patmos;
  switch (RegEnum) {
  case NoRegister: return 0; // Required for default P0 guard
  case R0:  case S0:  case P0:  return 0;
  case R1:  case S1:  case P1:  return 1;
  case R2:  case SL:  case P2:  return 2;
  case R3:  case SH:  case P3:  return 3;
  case R4:  case S4:  case P4:  return 4;
  case R5:  case SS:  case P5:  return 5;
  case R6:  case ST:  case P6:  return 6;
  case R7:  case SRB: case P7:  return 7;
  case R8:  case SRO: return 8;
  case R9:  case SXB: return 9;
  case R10: case SXO: return 10;
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
  case RTR: return 29;
  case RFP: return 30;
  case RSP: return 31;
  default:
    llvm_unreachable("Unknown Patmos register!");
  }
}

}

#endif
