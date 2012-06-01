//===-- PatmosASMBackend.cpp - Patmos Asm Backend  ----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the PatmosAsmBackend and PatmosELFObjectWriter classes.
//
//===----------------------------------------------------------------------===//
//

#include "PatmosFixupKinds.h"
#include "MCTargetDesc/PatmosMCTargetDesc.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

// Prepare value for the target space for it
static unsigned adjustFixupValue(unsigned Kind, uint64_t Value) {

  // Add/subtract and shift
  switch (Kind) {
  default:
    return 0;
  case FK_GPRel_4:
  case FK_Data_4:
    break;
/* TODO handle patmos specific fixups, handle ELF generic fixups if used.
  case Patmos::fixup_Patmos_PC16:
    // So far we are only using this type for branches.
    // For branches we start 1 instruction after the branch
    // so the displacement will be one instruction size less.
    Value -= 4;
    // The displacement is then divided by 4 to give us an 18 bit
    // address range.
    Value >>= 2;
    break;
  case Patmos::fixup_Patmos_26:
    // So far we are only using this type for jumps.
    // The displacement is then divided by 4 to give us an 28 bit
    // address range.
    Value >>= 2;
    break;
  case Patmos::fixup_Patmos_HI16:
  case Patmos::fixup_Patmos_GOT_Local:
    // Get the higher 16-bits. Also add 1 if bit 15 is 1.
    Value = ((Value + 0x8000) >> 16) & 0xffff;
    break;
*/
  }

  return Value;
}

namespace {
class PatmosAsmBackend : public MCAsmBackend {
  Triple::OSType OSType;

public:
  PatmosAsmBackend(const Target &T,  Triple::OSType _OSType)
    :MCAsmBackend(), OSType(_OSType) {}

  MCObjectWriter *createObjectWriter(raw_ostream &OS) const {
    return createPatmosELFObjectWriter(OS, OSType);
  }

  /// ApplyFixup - Apply the \arg Value for given \arg Fixup into the provided
  /// data fragment, at the offset specified by the fixup and following the
  /// fixup kind as appropriate.
  void applyFixup(const MCFixup &Fixup, char *Data, unsigned DataSize,
                  uint64_t Value) const {
    MCFixupKind Kind = Fixup.getKind();
    Value = adjustFixupValue((unsigned)Kind, Value);

    if (!Value)
      return; // Doesn't change encoding.

    // Where do we start in the object
    unsigned Offset = Fixup.getOffset();
    // Number of bytes we need to fixup
    unsigned NumBytes = (getFixupKindInfo(Kind).TargetSize + 7) / 8;
    // Used to point to big endian bytes
    unsigned FullSize;

    // TODO update this for various Patmos specific fixups
    // Offset + FullSize must point to the LSB of the fixup!
    switch ((unsigned)Kind) {
    case Patmos::FK_Patmos_22:
      FullSize = 2;
      break;
    default:
      FullSize = 4;
      break;
    }

    // Grab current value, if any, from bits.
    uint64_t CurVal = 0;

    for (unsigned i = 0; i != NumBytes; ++i) {
      unsigned Idx = (FullSize - 1 - i);
      CurVal |= (uint64_t)((uint8_t)Data[Offset + Idx]) << (i*8);
    }

    uint64_t Mask = ((uint64_t)(-1) >> (64 - getFixupKindInfo(Kind).TargetSize));
    CurVal |= Value & Mask;

    // Write out the fixed up bytes back to the code/data bits.
    for (unsigned i = 0; i != NumBytes; ++i) {
      unsigned Idx = (FullSize - 1 - i);
      Data[Offset + Idx] = (uint8_t)((CurVal >> (i*8)) & 0xff);
    }
  }

  unsigned getNumFixupKinds() const { return Patmos::NumTargetFixupKinds; }

  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const {

    // TODO define Patmos specific fixup infos
    const static MCFixupKindInfo Infos[Patmos::NumTargetFixupKinds] = {
      // This table *must* be in same the order of fixup_* kinds in
      // PatmosFixupKinds.h.
      //
      // name                    offset  bits  flags
      { "FK_Patmos_22",           0,     16,   0 }
      /* { "fixup_Patmos_PC16",         0,     16,  MCFixupKindInfo::FKF_IsPCRel }, */
    };

    if (Kind < FirstTargetFixupKind)
      return MCAsmBackend::getFixupKindInfo(Kind);

    assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
           "Invalid kind!");
    return Infos[Kind - FirstTargetFixupKind];
  }

  /// @name Target Relaxation Interfaces
  /// @{

  /// MayNeedRelaxation - Check whether the given instruction may need
  /// relaxation.
  ///
  /// \param Inst - The instruction to test.
  bool mayNeedRelaxation(const MCInst &Inst) const {
    // TODO return true for small immediates (?)
    return false;
  }

  /// fixupNeedsRelaxation - Target specific predicate for whether a given
  /// fixup requires the associated instruction to be relaxed.
  bool fixupNeedsRelaxation(const MCFixup &Fixup,
                            uint64_t Value,
                            const MCInstFragment *DF,
                            const MCAsmLayout &Layout) const
  {

    // TODO check for branch/call immediate
    assert(0 && "RelaxInstruction() unimplemented");
    return false;
  }

  /// RelaxInstruction - Relax the instruction in the given fragment
  /// to the next wider instruction.
  ///
  /// \param Inst - The instruction to relax, which may be the same
  /// as the output.
  /// \parm Res [output] - On return, the relaxed instruction.
  void relaxInstruction(const MCInst &Inst, MCInst &Res) const {
    // TODO relax small immediates (?)
  }

  /// @}

  /// WriteNopData - Write an (optimal) nop sequence of Count bytes
  /// to the given output. If the target cannot generate such a sequence,
  /// it should return an error.
  ///
  /// \return - True on success.
  bool writeNopData(uint64_t Count, MCObjectWriter *OW) const {

    // Count is in terms of of ValueSize, which is always 1 Byte for ELF.
    // This method is used to create a NOP slide for code segment alignment
    // OW handles byteorder stuff.

    if ((Count % 4) != 0)
      return false;

    // We should somehow initialize the NOP instruction code from TableGen, but
    // I do not see how (without creating a new CodeEmitter and everything from Target)

    for (uint64_t i = 0; i < Count; i += 4)
        OW->Write32(0x02200000);

    return true;
  }
}; // class PatmosAsmBackend

} // namespace

// MCAsmBackend
MCAsmBackend *llvm::createPatmosAsmBackend(const Target &T, StringRef TT) {
  return new PatmosAsmBackend(T, Triple(TT).getOS());
}

