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


#include "PatmosGenDisassemblerTables.inc"

  /// readInstruction - read four bytes from the MemoryObject
static DecodeStatus readInstruction32(const MemoryObject &region,
                                      uint64_t address,
                                      uint64_t &size,
                                      uint32_t &insn) {
  uint8_t Bytes[4];

  // We want to read exactly 4 Bytes of data.
  if (region.readBytes(address, 4, (uint8_t*)Bytes, NULL) == -1) {
    return MCDisassembler::Fail;
  }

  size += 4;

  // Encoded as a big-endian 32-bit word in the stream.
  insn = (Bytes[3] <<  0) |
         (Bytes[2] <<  8) |
         (Bytes[1] << 16) |
         (Bytes[0] << 24);

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

  Size = 0;

  DecodeStatus Result = readInstruction32(Region, Address, Size, Insn);
  if (Result == MCDisassembler::Fail)
    return MCDisassembler::Fail;

  // TODO we could check the opcode for ALUl instruction format to avoid calling decode32 in that case

  // Calling the auto-generated decoder function.
  Result = decodePatmosInstruction32(instr, Insn, Address, this, STI);

  bool isBundled = (Insn >> 31);

  // Try decoding as 64bit ALUl instruction
  if (Result == MCDisassembler::Fail) {
    if (!isBundled) return MCDisassembler::Fail;

    uint32_t InsnL;

    Result = readInstruction32(Region, Address + 4, Size, InsnL);
    if (Result == MCDisassembler::Fail) {
      return MCDisassembler::Fail;
    }

    uint64_t Insn64 = ((uint64_t)Insn << 32) | InsnL;

    Result = decodePatmosInstruction64(instr, Insn64, Address, this, STI);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;

  }

  // TODO handle bundled instructions somehow

  return Result;
}



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

