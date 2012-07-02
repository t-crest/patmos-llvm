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
#include "llvm/MC/MCValue.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCELFSymbolFlags.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ELF.h"

using namespace llvm;
using namespace Patmos;

// Prepare value for the target space for it
static unsigned adjustFixupValue(unsigned Kind, uint64_t Value) {

  // Add/subtract and shift
  switch (Kind) {
  // TODO check: do we need to shift the load/store offsets here or is this done earlier in the compiler?
  case FK_Patmos_HO_7:
    Value >>= 1;
    break;
  case FK_Patmos_WO_7:
  case FK_Patmos_22:
  case FK_Patmos_frel_22:
    Value >>= 2;
    break;
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

  const MCSymbolData &getFunctionSymbol(const MCAssembler &Asm,
                                        const MCAsmLayout &Layout, const MCSymbol &Symbol,
                                        const MCSymbolData &SD, const MCFragment *DF)
  {
    const MCSection *Section = &Symbol.getSection();
    uint64_t Offset = Layout.getFragmentOffset(DF) + SD.getOffset();

    const MCSymbolData *Curr = 0;
    uint64_t CurrOff = 0;

    // TODO this is slow.. maybe sort the symbol table by section and offset once, then iterate backwards
    // until first function symbol is found

    for (MCAssembler::const_symbol_iterator it = Asm.symbol_begin(), end = Asm.symbol_end(); it != end; ++it) {
      const MCSymbolData &SymD = *it;
      if (!SymD.getSymbol().isInSection()) continue;
      if (&SymD.getSymbol().getSection() != Section) continue;
      if ((SymD.getFlags() & ELF_STT_Func) == 0) continue;

      uint64_t SymOff = Layout.getFragmentOffset(SymD.getFragment()) + SymD.getOffset();

      if (SymOff > Offset) continue;

      if (CurrOff <= SymOff) {
        Curr = &SymD;
        CurrOff = SymOff;
      }
    }

    return *Curr;
  }

  virtual void processFixupValue(const MCAssembler &Asm,
                                 const MCAsmLayout &Layout,
                                 const MCFixup &Fixup, const MCFragment *DF,
                                 MCValue &Target, uint64_t &Value,
                                 bool &IsResolved)
  {
    // TODO check if we can resolve the fixup, and if so, do so

    unsigned Kind = (unsigned)Fixup.getKind();

    // For FRELs, we need to adjust the offset value to the start of the current (sub)function
    if (isFRELFixupKind(Kind)) {
      assert(!Target.getSymB() && "FREL fixup kind must not be a relative symbol");
      const MCSymbol &Symbol = Target.getSymA()->getSymbol().AliasedSymbol();
      const MCSymbolData &SDA = Asm.getSymbolData(Symbol);

      const MCSymbolData &SDF = getFunctionSymbol(Asm, Layout, Symbol, SDA, DF);

      uint64_t Offset = Layout.getSymbolOffset(&SDA) - Layout.getSymbolOffset(&SDF);

      const MCSymbolRefExpr *FnSymRef = MCSymbolRefExpr::Create(&SDF.getSymbol(), Asm.getContext());

      Target = MCValue::get(FnSymRef, 0, Offset);
      Value = Offset;

      return;
    }

    // TODO in case of FK_Patmos_BO|HO|WO_7, we can (try to) resolve it here, and skip emitting relocations

  }

  /// ApplyFixup - Apply the \arg Value for given \arg Fixup into the provided
  /// data fragment, at the offset specified by the fixup and following the
  /// fixup kind as appropriate.
  virtual void applyFixup(const MCFixup &Fixup, char *Data, unsigned DataSize,
                  uint64_t Value) const {
    MCFixupKind Kind = Fixup.getKind();

    // Adjust the immediate value according to the format here
    // this is not done in processFixupValue to avoid keeping track of the current addressing mode in the rest of the code
    Value = adjustFixupValue(Kind, Value);

    if (!Value)
      return; // Doesn't change encoding.

    unsigned TargetSize = getFixupKindInfo(Kind).TargetSize;
    unsigned TargetOffset = getFixupKindInfo(Kind).TargetOffset;

    // Where do we start in the object
    unsigned Offset = Fixup.getOffset();
    // Number of bytes we need to fixup
    unsigned NumBytes = (TargetSize + 7) / 8;
    // Used to point to big endian bytes
    unsigned FullSize = (TargetOffset + TargetSize + 7) / 8;

    // Grab current value, if any, from bits.
    uint64_t CurVal = 0;

    for (unsigned i = 0; i != NumBytes; ++i) {
      unsigned Idx = (FullSize - 1 - i);
      CurVal |= (uint64_t)((uint8_t)Data[Offset + Idx]) << (i*8);
    }

    uint64_t Mask = ((uint64_t)(-1) >> (64 - TargetSize));
    unsigned Shift = FullSize * 8 - (TargetOffset + TargetSize);

    CurVal |= (Value & Mask) << Shift;

    // Write out the fixed up bytes back to the code/data bits.
    for (unsigned i = 0; i != NumBytes; ++i) {
      unsigned Idx = (FullSize - 1 - i);
      Data[Offset + Idx] = (uint8_t)((CurVal >> (i*8)) & 0xff);
    }
  }

  unsigned getNumFixupKinds() const { return Patmos::NumTargetFixupKinds; }

  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const {

    const static MCFixupKindInfo Infos[Patmos::NumTargetFixupKinds] = {
      // This table *must* be in same the order of FK_* kinds in
      // PatmosFixupKinds.h.
      //
      // name                    offset  bits  flags
      { "FK_Patmos_BO_7" ,       25,      7,   0 }, // 0 bit shifted, signed (byte aligned)
      { "FK_Patmos_SO_7" ,       25,      7,   0 }, // 1 bit shifted, signed (half-word aligned)
      { "FK_Patmos_WO_7" ,       25,      7,   0 }, // 2 bit shifted, signed (word aligned)
      { "FK_Patmos_12",          20,     12,   0 }, // ALU immediate, unsigned
      { "FK_Patmos_22",          10,     22,   0 }, // 2 bit shifted, unsigned, for call
      { "FK_Patmos_stc_22",      10,     22,   0 }, // 2 bit shifted, unsigned, for stack control
      { "FK_Patmos_32",          32,     32,   0 }, // ALU immediate, unsigned
      { "FK_Patmos_frel_12",     20,     12,   MCFixupKindInfo::FKF_IsPCRel }, // 0 bit shifted, signed, function relative
      { "FK_Patmos_frel_22",     10,     22,   MCFixupKindInfo::FKF_IsPCRel }, // 2 bit shifted, signed, function relative
      { "FK_Patmos_frel_32",      0,     32,   MCFixupKindInfo::FKF_IsPCRel }, // 0 bit shifted, signed, function relative
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
        OW->Write32(0x02400000);

    return true;
  }
}; // class PatmosAsmBackend

} // namespace

// MCAsmBackend
MCAsmBackend *llvm::createPatmosAsmBackend(const Target &T, StringRef TT) {
  return new PatmosAsmBackend(T, Triple(TT).getOS());
}

