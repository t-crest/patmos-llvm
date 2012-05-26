//===- PatmosDisassembler.cpp - Disassembler for Patmos -------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is part of the Patmos Disassembler.
//
//===----------------------------------------------------------------------===//

#include "Patmos.h"
#include "PatmosSubtarget.h"
#include "llvm/MC/EDInstInfo.h"
#include "llvm/MC/MCDisassembler.h"
#include "llvm/Support/MemoryObject.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/MathExtras.h"


#include "PatmosGenEDInfo.inc"

using namespace llvm;

typedef MCDisassembler::DecodeStatus DecodeStatus;

/// PatmosDisassembler - a disassembler class for Patmos.
class PatmosDisassembler : public MCDisassembler {
public:
  /// Constructor     - Initializes the disassembler.
  ///
  PatmosDisassembler(const MCSubtargetInfo &STI) :
    MCDisassembler(STI) {
  }

  ~PatmosDisassembler() {
  }

  /// getInstruction - See MCDisassembler.
  DecodeStatus getInstruction(MCInst &instr,
                              uint64_t &size,
                              const MemoryObject &region,
                              uint64_t address,
                              raw_ostream &vStream,
                              raw_ostream &cStream) const;

  /// getEDInfo - See MCDisassembler.
  const EDInstInfo *getEDInfo() const;

};


const EDInstInfo *PatmosDisassembler::getEDInfo() const {
  return instInfoPatmos;
}


// Forward declare these because the autogenerated code will reference them.
// Definitions are further down.

static DecodeStatus DecodeCPURegsRegisterClass(MCInst &Inst,
                                               unsigned RegNo,
                                               uint64_t Address,
                                               const void *Decoder);

static DecodeStatus DecodeCCRRegisterClass(MCInst &Inst,
                                           unsigned RegNo,
                                           uint64_t Address,
                                           const void *Decoder);

static DecodeStatus DecodeHWRegsRegisterClass(MCInst &Inst,
                                              unsigned Insn,
                                              uint64_t Address,
                                              const void *Decoder);

static DecodeStatus DecodeBranchTarget(MCInst &Inst,
                                       unsigned Offset,
                                       uint64_t Address,
                                       const void *Decoder);

static DecodeStatus DecodeBC1(MCInst &Inst,
                              unsigned Insn,
                              uint64_t Address,
                              const void *Decoder);


static DecodeStatus DecodeJumpTarget(MCInst &Inst,
                                     unsigned Insn,
                                     uint64_t Address,
                                     const void *Decoder);

static DecodeStatus DecodeMem(MCInst &Inst,
                              unsigned Insn,
                              uint64_t Address,
                              const void *Decoder);

static DecodeStatus DecodeFMem(MCInst &Inst, unsigned Insn,
                               uint64_t Address,
                               const void *Decoder);

static DecodeStatus DecodeSimm16(MCInst &Inst,
                                 unsigned Insn,
                                 uint64_t Address,
                                 const void *Decoder);

static DecodeStatus DecodeCondCode(MCInst &Inst,
                                   unsigned Insn,
                                   uint64_t Address,
                                   const void *Decoder);

static DecodeStatus DecodeInsSize(MCInst &Inst,
                                  unsigned Insn,
                                  uint64_t Address,
                                  const void *Decoder);

static DecodeStatus DecodeExtSize(MCInst &Inst,
                                  unsigned Insn,
                                  uint64_t Address,
                                  const void *Decoder);

namespace llvm {
extern Target ThePatmosTarget;
}

static MCDisassembler *createPatmosDisassembler(
                       const Target &T,
                       const MCSubtargetInfo &STI) {
  return new PatmosDisassembler(STI);
}

extern "C" void LLVMInitializePatmosDisassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(ThePatmosTarget,
                                         createPatmosDisassembler);
}


#include "PatmosGenDisassemblerTables.inc"

  /// readInstruction - read four bytes from the MemoryObject
static DecodeStatus readInstruction(const MemoryObject &region,
                                      uint64_t address,
                                      uint64_t &size,
                                      uint32_t &insn) {
  uint8_t Bytes[4];

  // We want to read exactly 4 Bytes of data.
  if (region.readBytes(address, 4, (uint8_t*)Bytes, NULL) == -1) {
    size = 0;
    return MCDisassembler::Fail;
  }

  if (isBigEndian) {
    // Encoded as a big-endian 32-bit word in the stream.
    insn = (Bytes[3] <<  0) |
           (Bytes[2] <<  8) |
           (Bytes[1] << 16) |
           (Bytes[0] << 24);
  }
  else {
    // Encoded as a small-endian 32-bit word in the stream.
    insn = (Bytes[0] <<  0) |
           (Bytes[1] <<  8) |
           (Bytes[2] << 16) |
           (Bytes[3] << 24);
  }

  return MCDisassembler::Success;
}

DecodeStatus
PatmosDisassembler::getInstruction(MCInst &instr,
                                 uint64_t &Size,
                                 const MemoryObject &Region,
                                 uint64_t Address,
                                 raw_ostream &vStream,
                                 raw_ostream &cStream) const {
  uint32_t Insn;

  DecodeStatus Result = readInstruction(Region, Address, Size,
                                          Insn, isBigEndian);
  if (Result == MCDisassembler::Fail)
    return MCDisassembler::Fail;

  // Calling the auto-generated decoder function.
  Result = decodePatmosInstruction(instr, Insn, Address, this, STI);
  if (Result != MCDisassembler::Fail) {
    Size = 4;
    return Result;
  }

  return MCDisassembler::Fail;
}


static DecodeStatus DecodeCPURegsRegisterClass(MCInst &Inst,
                                               unsigned RegNo,
                                               uint64_t Address,
                                               const void *Decoder) {
  if (RegNo > 31)
    return MCDisassembler::Fail;

  Inst.addOperand(MCOperand::CreateReg(RegNo));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeCCRRegisterClass(MCInst &Inst,
                                           unsigned RegNo,
                                           uint64_t Address,
                                           const void *Decoder) {
  Inst.addOperand(MCOperand::CreateReg(RegNo));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeMem(MCInst &Inst,
                              unsigned Insn,
                              uint64_t Address,
                              const void *Decoder) {
  int Offset = SignExtend32<16>(Insn & 0xffff);
  int Reg = (int)fieldFromInstruction32(Insn, 16, 5);
  int Base = (int)fieldFromInstruction32(Insn, 21, 5);

  if(Inst.getOpcode() == Patmos::SC){
    Inst.addOperand(MCOperand::CreateReg(CPURegsTable[Reg]));
  }

  Inst.addOperand(MCOperand::CreateReg(CPURegsTable[Reg]));
  Inst.addOperand(MCOperand::CreateReg(CPURegsTable[Base]));
  Inst.addOperand(MCOperand::CreateImm(Offset));

  return MCDisassembler::Success;
}

static DecodeStatus DecodeFMem(MCInst &Inst,
                               unsigned Insn,
                               uint64_t Address,
                               const void *Decoder) {
  int Offset = SignExtend32<16>(Insn & 0xffff);
  int Reg = (int)fieldFromInstruction32(Insn, 16, 5);
  int Base = (int)fieldFromInstruction32(Insn, 21, 5);

  Inst.addOperand(MCOperand::CreateReg(FGR64RegsTable[Reg]));
  Inst.addOperand(MCOperand::CreateReg(CPURegsTable[Base]));
  Inst.addOperand(MCOperand::CreateImm(Offset));

  return MCDisassembler::Success;
}


static DecodeStatus DecodeCondCode(MCInst &Inst,
                                   unsigned Insn,
                                   uint64_t Address,
                                   const void *Decoder) {
  int CondCode = Insn & 0xf;
  Inst.addOperand(MCOperand::CreateImm(CondCode));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeBranchTarget(MCInst &Inst,
                                       unsigned Offset,
                                       uint64_t Address,
                                       const void *Decoder) {
  unsigned BranchOffset = Offset & 0xffff;
  BranchOffset = SignExtend32<18>(BranchOffset << 2) + 4;
  Inst.addOperand(MCOperand::CreateImm(BranchOffset));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeJumpTarget(MCInst &Inst,
                                     unsigned Insn,
                                     uint64_t Address,
                                     const void *Decoder) {

  unsigned JumpOffset = fieldFromInstruction32(Insn, 0, 26) << 2;
  Inst.addOperand(MCOperand::CreateImm(JumpOffset));
  return MCDisassembler::Success;
}


static DecodeStatus DecodeSimm16(MCInst &Inst,
                                 unsigned Insn,
                                 uint64_t Address,
                                 const void *Decoder) {
  Inst.addOperand(MCOperand::CreateImm(SignExtend32<16>(Insn)));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeInsSize(MCInst &Inst,
                                  unsigned Insn,
                                  uint64_t Address,
                                  const void *Decoder) {
  // First we need to grab the pos(lsb) from MCInst.
  int Pos = Inst.getOperand(2).getImm();
  int Size = (int) Insn - Pos + 1;
  Inst.addOperand(MCOperand::CreateImm(SignExtend32<16>(Size)));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeExtSize(MCInst &Inst,
                                  unsigned Insn,
                                  uint64_t Address,
                                  const void *Decoder) {
  int Size = (int) Insn  + 1;
  Inst.addOperand(MCOperand::CreateImm(SignExtend32<16>(Size)));
  return MCDisassembler::Success;
}
