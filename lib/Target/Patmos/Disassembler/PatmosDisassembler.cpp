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

#define DEBUG_TYPE "patmos-disassembler"

#include "Patmos.h"
#include "PatmosSubtarget.h"
#include "MCTargetDesc/PatmosBaseInfo.h"
#include "llvm/MC/MCDisassembler.h"
#include "llvm/MC/MCFixedLenDisassembler.h"
#include "llvm/Support/MemoryObject.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

typedef MCDisassembler::DecodeStatus DecodeStatus;

/// PatmosDisassembler - a disassembler class for Patmos.
class PatmosDisassembler : public MCDisassembler {
  MCInstrInfo *MII;
public:
  /// Constructor     - Initializes the disassembler.
  ///
  PatmosDisassembler(const Target &T, const MCSubtargetInfo &STI, MCContext &Ctx) :
    MCDisassembler(STI, Ctx), MII(T.createMCInstrInfo()) {
  }

  ~PatmosDisassembler() {
    if (MII) delete MII;
    MII = 0;
  }

  /// getInstruction - See MCDisassembler.
  DecodeStatus getInstruction(MCInst &instr,
                              uint64_t &Size,
                              ArrayRef<uint8_t> Bytes,
                              uint64_t Address,
                              raw_ostream &vStream,
                              raw_ostream &cStream) const;

private:

  /// adjustSignedImm - convert immediates to signed by sign-extend if necessary
  void adjustSignedImm(MCInst &instr) const;

};

// We could use the information from PatmosGenRegisterInfo.inc here,
// but this would require linking to PatmosMCTargetDesc and providing
// static functions there to access those structs.
static const unsigned RRegsTable[] = {
    Patmos::R0,  Patmos::R1,  Patmos::R2,  Patmos::R3,
    Patmos::R4,  Patmos::R5,  Patmos::R6,  Patmos::R7,
    Patmos::R8,  Patmos::R9,  Patmos::R10, Patmos::R11,
    Patmos::R12, Patmos::R13, Patmos::R14, Patmos::R15,
    Patmos::R16, Patmos::R17, Patmos::R18, Patmos::R19,
    Patmos::R20, Patmos::R21, Patmos::R22, Patmos::R23,
    Patmos::R24, Patmos::R25, Patmos::R26, Patmos::R27,
    Patmos::R28, Patmos::RTR, Patmos::RFP, Patmos::RSP,
};
static const unsigned SRegsTable[] = {
    Patmos::S0,  Patmos::S1,  Patmos::SL,  Patmos::SH,
    Patmos::S4,  Patmos::SS,  Patmos::ST,  Patmos::SRB,
    Patmos::SRO, Patmos::SXB, Patmos::SXO, Patmos::S11,
    Patmos::S12, Patmos::S13, Patmos::S14, Patmos::S15,
};
static const unsigned PRegsTable[] = {
    Patmos::P0,  Patmos::P1,  Patmos::P2,  Patmos::P3,
    Patmos::P4,  Patmos::P5,  Patmos::P6,  Patmos::P7
};


// Forward declarations for tablegen'd decoder
static DecodeStatus DecodeRRegsRegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address,
                                             const void *Decoder);
static DecodeStatus DecodeSRegsRegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address,
                                             const void *Decoder);
static DecodeStatus DecodePRegsRegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address,
                                             const void *Decoder);
static DecodeStatus DecodePredRegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address,
                                            const void *Decoder);

#include "PatmosGenDisassemblerTables.inc"

/// readInstruction - read four bytes from the MemoryObject
static DecodeStatus readInstruction32(ArrayRef<uint8_t> Bytes,
                                      uint64_t Offset,
                                      uint64_t &Size,
                                      uint32_t &Insn) {
  // We want to read exactly 4 Bytes of data.
  if (Bytes.size() < Offset+4) {
    Size = 0;
    return MCDisassembler::Fail;
  }

  Size += 4;

  // Encoded as a big-endian 32-bit word in the stream.
  Insn = (Bytes[Offset+3] <<  0) |
         (Bytes[Offset+2] <<  8) |
         (Bytes[Offset+1] << 16) |
         (Bytes[Offset+0] << 24);

  return MCDisassembler::Success;
}

DecodeStatus
PatmosDisassembler::getInstruction(MCInst &instr,
                                   uint64_t &Size,
                                   ArrayRef<uint8_t> Bytes,
                                   uint64_t Address,
                                   raw_ostream &vStream,
                                   raw_ostream &cStream) const {
  uint32_t Insn;

  Size = 0;

  DecodeStatus Result = readInstruction32(Bytes, 0, Size, Insn);
  if (Result == MCDisassembler::Fail)
    return MCDisassembler::Fail;

  bool isBundled = (Insn >> 31);
  Insn &= ~(1<<31);

  // TODO we could check the opcode for ALUl instruction format to avoid calling decode32 in that case

  // Calling the auto-generated decoder function.
  Result = decodeInstruction(DecoderTablePatmos32, instr, Insn, Address, this, STI);

  // Try decoding as 64bit ALUl instruction
  if (Result == MCDisassembler::Fail) {
    if (!isBundled) return MCDisassembler::Fail;

    uint32_t InsnL;

    Result = readInstruction32(Bytes, 4, Size, InsnL);
    if (Result == MCDisassembler::Fail) {
      return MCDisassembler::Fail;
    }

    // Set bundle-bit for ALUl format, combine instruction opcode and immediate
    uint64_t Insn64 = (1ULL << 63) | ((uint64_t)Insn << 32) | InsnL;

    Result = decodeInstruction(DecoderTablePatmos64, instr, Insn64, Address, this, STI);
    if (Result == MCDisassembler::Fail)
      return MCDisassembler::Fail;

    // If we have a 64bit instruction, do not mark instruction as bundled
    isBundled = false;
  }

  // handle bundled instructions by adding a special operand
  instr.addOperand(MCOperand::createImm(isBundled));

  adjustSignedImm(instr);

  return Result;
}

void PatmosDisassembler::adjustSignedImm(MCInst &instr) const {

  const MCInstrDesc &MID = MII->get(instr.getOpcode());

  bool ImmSigned = isPatmosImmediateSigned(MID.TSFlags);

  if (hasPatmosImmediate(MID.TSFlags) && ImmSigned) {
    unsigned ImmOpNo = getPatmosImmediateOpNo(MID.TSFlags);
    unsigned ImmSize = getPatmosImmediateSize(MID.TSFlags);

    uint64_t Value = instr.getOperand(ImmOpNo).getImm();

    if (Value & (1 << (ImmSize-1))) {

      // sign bit is set => sign-extend
      Value |= ~0ULL ^ ((1ULL << ImmSize) - 1);

      instr.getOperand(ImmOpNo).setImm(Value);
    }
  }

}

static DecodeStatus DecodeRRegsRegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address,
                                             const void *Decoder)
{
  if (RegNo > 31)
    return MCDisassembler::Fail;

  Inst.addOperand(MCOperand::createReg(RRegsTable[RegNo]));

  return MCDisassembler::Success;
}

static DecodeStatus DecodeSRegsRegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address,
                                             const void *Decoder)
{
  if (RegNo > 15)
    return MCDisassembler::Fail;

  Inst.addOperand(MCOperand::createReg(SRegsTable[RegNo]));

  return MCDisassembler::Success;
}

static DecodeStatus DecodePRegsRegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address,
                                             const void *Decoder)
{
  if (RegNo > 7)
    return MCDisassembler::Fail;

  Inst.addOperand(MCOperand::createReg(PRegsTable[RegNo]));

  return MCDisassembler::Success;
}

static DecodeStatus DecodePredRegisterClass(MCInst &Inst, unsigned RegNo, uint64_t Address,
                                            const void *Decoder)
{
  bool flag    = RegNo >> 3;
  unsigned reg = RegNo & 0x07;

  Inst.addOperand(MCOperand::createReg(PRegsTable[reg]));
  Inst.addOperand(MCOperand::createImm(flag));

  return MCDisassembler::Success;
}


namespace llvm {
extern Target ThePatmosTarget;
}

static MCDisassembler *createPatmosDisassembler(
                       const Target &T,
                       const MCSubtargetInfo &STI,
                       MCContext &Ctx) {
  return new PatmosDisassembler(T, STI, Ctx);
}

extern "C" void LLVMInitializePatmosDisassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(ThePatmosTarget,
                                         createPatmosDisassembler);
}

