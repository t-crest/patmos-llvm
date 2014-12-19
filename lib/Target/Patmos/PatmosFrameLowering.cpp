//======-- PatmosFrameLowering.cpp - Patmos Frame Information -------=========//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Patmos implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-framelowering"
#include "PatmosFrameLowering.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSinglePathInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Function.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

namespace llvm {
  /// Count the number of FIs overflowing into the shadow stack
  STATISTIC(FIsNotFitSC, "FIs that did not fit in the stack cache");
}

/// DisableStackCache - Command line option to disable the usage of the stack 
/// cache (enabled by default).
static cl::opt<bool> DisableStackCache("mpatmos-disable-stack-cache",
                            cl::init(false),
                            cl::desc("Disable the use of Patmos' stack cache"));

/// EnableBlockAlignedStackCache - Command line option to enable the usage of 
/// the block-aligned stack cache (disabled by default).
static cl::opt<bool> EnableBlockAlignedStackCache
          ("mpatmos-enable-block-aligned-stack-cache", cl::init(false),
           cl::desc("Enable the use of Patmos' block-aligned stack cache"));


PatmosFrameLowering::PatmosFrameLowering(const PatmosTargetMachine &tm)
: TargetFrameLowering(TargetFrameLowering::StackGrowsDown, 4, 0), TM(tm),
  STC(tm.getSubtarget<PatmosSubtarget>())
{
}



bool PatmosFrameLowering::hasFP(const MachineFunction &MF) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();

  // Naked functions should not use the stack, they do not get a frame pointer.
  if (MF.getFunction()->hasFnAttribute(Attribute::Naked))
    return false;

  return (MF.getTarget().Options.DisableFramePointerElim(MF) ||
          MF.getFrameInfo()->hasVarSizedObjects() ||
          MFI->isFrameAddressTaken());
}


#if 0
bool PatmosFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  return !MF.getFrameInfo()->hasVarSizedObjects();
}
#endif


static unsigned int align(unsigned int offset, unsigned int alignment) {
  return ((offset + alignment - 1) / alignment) * alignment;
}

unsigned PatmosFrameLowering::getEffectiveStackCacheSize() const
{
  return EnableBlockAlignedStackCache ? 
                        STC.getStackCacheSize() - STC.getStackCacheBlockSize() : 
                        STC.getStackCacheSize();
}

unsigned PatmosFrameLowering::getEffectiveStackCacheBlockSize() const
{
  return EnableBlockAlignedStackCache ? 4 : STC.getStackCacheBlockSize();
}

unsigned PatmosFrameLowering::getAlignedStackCacheFrameSize(
                                                       unsigned frameSize) const
{
  if (frameSize == 0)
    return frameSize;
  else
    return EnableBlockAlignedStackCache ? frameSize : 
                                        STC.getAlignedStackFrameSize(frameSize);
}

void PatmosFrameLowering::assignFIsToStackCache(MachineFunction &MF,
                                                BitVector &SCFIs) const
{
  MachineFrameInfo &MFI = *MF.getFrameInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  const TargetRegisterInfo *TRI = TM.getRegisterInfo();

  assert(MFI.isCalleeSavedInfoValid());

  // find all FIs used for callee saved registers
  for(std::vector<CalleeSavedInfo>::const_iterator i(CSI.begin()),
      ie(CSI.end()); i != ie; i++)
  {
    if (i->getReg() == Patmos::S0 && PMFI.getS0SpillReg()) continue;
    // Predicates are handled via aliasing to S0. They appear here when we
    // skip assigning s0 to a stack slot, not really sure why.
    if (Patmos::PRegsRegClass.contains(i->getReg())) continue;
    SCFIs[i->getFrameIdx()] = true;
  }

  // RegScavenging register
  if (TRI->requiresRegisterScavenging(MF)) {
    SCFIs[PMFI.getRegScavengingFI()] = true;
  }

  // Spill slots / storage introduced for single path conversion
  const std::vector<int> &SinglePathFIs = PMFI.getSinglePathFIs();
  for(unsigned i=0; i<SinglePathFIs.size(); i++) {
    SCFIs[SinglePathFIs[i]] = true;
  }

  // find all FIs that are spill slots
  for(unsigned FI = 0, FIe = MFI.getObjectIndexEnd(); FI != FIe; FI++) {
    if (MFI.isDeadObjectIndex(FI))
      continue;

    // find all spill slots and locations for callee saved registers
    if (MFI.isSpillSlotObjectIndex(FI))
      SCFIs[FI] = true;
  }
}



unsigned PatmosFrameLowering::assignFrameObjects(MachineFunction &MF,
                                                 bool UseStackCache) const
{
  MachineFrameInfo &MFI = *MF.getFrameInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();
  unsigned maxFrameSize = MFI.getMaxCallFrameSize();

  // defaults to false (all objects are assigned to shadow stack)
  BitVector SCFIs(MFI.getObjectIndexEnd());

  if (UseStackCache) {
    assignFIsToStackCache(MF, SCFIs);
  }

  // assign new offsets to FIs

  // next stack slot in stack cache
  unsigned int SCOffset = 0;
  // next stack slot in shadow stack
  // Also reserve space for the call frame if we do not use a frame pointer.
  // This must be in sync with PatmosRegisterInfo::eliminateCallFramePseudoInstr
  unsigned int SSOffset = (hasFP(MF) ? 0 : maxFrameSize);

  DEBUG(dbgs() << "PatmosSC: " << MF.getFunction()->getName() << "\n");
  DEBUG(MFI.print(MF, dbgs()));
  for(unsigned FI = 0, FIe = MFI.getObjectIndexEnd(); FI != FIe; FI++) {
    if (MFI.isDeadObjectIndex(FI))
      continue;

    unsigned FIalignment = MFI.getObjectAlignment(FI);
    int64_t FIsize = MFI.getObjectSize(FI);

    if (FIsize > INT_MAX) {
      report_fatal_error("Frame objects with size > INT_MAX not supported.");
    }

    // be sure to catch some special stack objects not expected for Patmos
    assert(!MFI.isFixedObjectIndex(FI) && !MFI.isObjectPreAllocated(FI));

    // assigned to stack cache or shadow stack?
    if (SCFIs[FI]) {
      // alignment
      unsigned int next_SCOffset = align(SCOffset, FIalignment);

      // check if the FI still fits into the SC
      if (align(next_SCOffset + FIsize, getEffectiveStackCacheBlockSize()) <=
          getEffectiveStackCacheSize()) {
        DEBUG(dbgs() << "PatmosSC: FI: " << FI << " on SC: " << next_SCOffset
                    << "(" << MFI.getObjectOffset(FI) << ")\n");

        // reassign stack offset
        MFI.setObjectOffset(FI, next_SCOffset);

        // reserve space on the stack cache
        SCOffset = next_SCOffset + FIsize;

        // the FI is assigned to the SC, process next FI
        continue;
      }
      else {
        // the FI did not fit in the SC -- fall-through and put it on the 
        // shadow stack
        SCFIs[FI] = false;
        FIsNotFitSC++;
      }
    }

    // assign the FI to the shadow stack
    {
      assert(!SCFIs[FI]);

      // alignment
      SSOffset = align(SSOffset, FIalignment);

      DEBUG(dbgs() << "PatmosSC: FI: " << FI << " on SS: " << SSOffset
                   << "(" << MFI.getObjectOffset(FI) << ")\n");

      // reassign stack offset
      MFI.setObjectOffset(FI, SSOffset);

      // reserve space on the shadow stack
      SSOffset += FIsize;
    }
  }

  // align stack frame on stack cache
  unsigned stackCacheSize = align(SCOffset, getEffectiveStackCacheBlockSize());

  assert(stackCacheSize <= getEffectiveStackCacheSize());

  // align shadow stack. call arguments are already included in SSOffset
  unsigned stackSize = align(SSOffset, getStackAlignment());

  // update offset of fixed objects
  for(unsigned FI = MFI.getObjectIndexBegin(), FIe = 0; FI != FIe; FI++) {
    // reassign stack offset
    MFI.setObjectOffset(FI, MFI.getObjectOffset(FI) + stackSize);
  }

  DEBUG(MFI.print(MF, dbgs()));

  // store assignment information
  PMFI.setStackCacheReservedBytes(stackCacheSize);
  PMFI.setStackCacheFIs(SCFIs);

  PMFI.setStackReservedBytes(stackSize);
  MFI.setStackSize(stackSize);

  return stackSize;
}



MachineInstr *PatmosFrameLowering::emitSTC(MachineFunction &MF, MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator &MI,
                                  unsigned Opcode) const {
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  // align the stack cache frame size
  unsigned alignedStackSize = getAlignedStackCacheFrameSize(
                                     PMFI.getStackCacheReservedBytes());
  assert(alignedStackSize <= getEffectiveStackCacheSize());

  // STC instructions are specified in words
  unsigned stackFrameSize = alignedStackSize / 4;

  MachineInstr *Inst = NULL;

  if (stackFrameSize) {
    assert(isUInt<22>(stackFrameSize) && "Stack cache size exceeded.");

    DebugLoc DL = (MI != MBB.end()) ? MI->getDebugLoc() : DebugLoc();

    const TargetInstrInfo &TII = *TM.getInstrInfo();

    // emit reserve instruction
    Inst = AddDefaultPred(BuildMI(MBB, MI, DL, TII.get(Opcode)))
      .addImm(stackFrameSize);
  }
  return Inst;
}

void PatmosFrameLowering::patchCallSites(MachineFunction &MF) const {
  // visit all basic blocks
  for (MachineFunction::iterator i(MF.begin()), ie(MF.end()); i != ie; ++i) {
    for (MachineBasicBlock::iterator j(i->begin()), je=(i->end()); j != je;
         j++) {
      // a call site?
      if (j->isCall()) {
        MachineBasicBlock::iterator p(llvm::next(j));
        emitSTC(MF, *i, p, Patmos::SENSi);
      }
    }
  }
}




void PatmosFrameLowering::emitPrologue(MachineFunction &MF) const {
  // get some references
  MachineBasicBlock &MBB     = MF.front();
  const TargetInstrInfo *TII = TM.getInstrInfo();

  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  //----------------------------------------------------------------------------
  // Handle the stack cache -- if enabled.

  // XXX this used to protect against issues with the alignment of large
  // (greater 4 bytes) objects, which is now guarded against in
  // PatmosTargetLowering::LowerCCCArguments().
#if 0
  MachineFrameInfo *MFI      = MF.getFrameInfo();
  if (MFI->getMaxAlignment() > 4) {
    dbgs() << "Stack alignment ";
    if (MF.getFunction()) dbgs() << "in " << MF.getFunction()->getName() << " ";
    dbgs() << "too large (" << MFI->getMaxAlignment() << ").\n";

    report_fatal_error("Stack alignment other than 4 byte is not supported");
  }
#endif

  // assign some FIs to the stack cache if possible
  unsigned stackSize = assignFrameObjects(MF, !DisableStackCache);

  if (!DisableStackCache) {
    // emit a reserve instruction
    MachineInstr *MI = emitSTC(MF, MBB, MBBI, Patmos::SRESi);
    if (MI) MI->setFlag(MachineInstr::FrameSetup);

    // patch all call sites
    patchCallSites(MF);
  }

  //----------------------------------------------------------------------------
  // Handle the shadow stack


  // Do we need to allocate space on the stack?
  if (stackSize) {
    // adjust stack : sp -= stack size
    if (stackSize <= 0xFFF) {
      MachineInstr *MI = AddDefaultPred(BuildMI(MBB, MBBI, dl,
            TII->get(Patmos::SUBi), Patmos::RSP))
        .addReg(Patmos::RSP).addImm(stackSize);
      MI->setFlag(MachineInstr::FrameSetup);

    }
    else {
      MachineInstr *MI = AddDefaultPred(BuildMI(MBB, MBBI, dl,
            TII->get(Patmos::SUBl), Patmos::RSP))
        .addReg(Patmos::RSP).addImm(stackSize);
      MI->setFlag(MachineInstr::FrameSetup);
    }
  }

}

void PatmosFrameLowering::emitEpilogue(MachineFunction &MF,
                                       MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  MachineFrameInfo *MFI            = MF.getFrameInfo();
  const TargetInstrInfo *TII       = TM.getInstrInfo();
  DebugLoc dl                      = MBBI->getDebugLoc();

  //----------------------------------------------------------------------------
  // Handle Stack Cache

  // emit a free instruction
  MachineInstr *MI = emitSTC(MF, MBB, MBBI, Patmos::SFREEi);
  if (MI) MI->setFlag(MachineInstr::FrameSetup);

  //----------------------------------------------------------------------------
  // Handle Shadow Stack

  // Get the number of bytes from FrameInfo
  unsigned stackSize = MFI->getStackSize();

  // adjust stack  : sp += stack size
  if (stackSize) {
    if (stackSize <= 0xFFF) {
      MachineInstr *MI = AddDefaultPred(BuildMI(MBB, MBBI, dl,
            TII->get(Patmos::ADDi), Patmos::RSP))
        .addReg(Patmos::RSP).addImm(stackSize);
      MI->setFlag(MachineInstr::FrameSetup);
    }
    else {
      MachineInstr *MI = AddDefaultPred(BuildMI(MBB, MBBI, dl,
            TII->get(Patmos::ADDl), Patmos::RSP))
        .addReg(Patmos::RSP).addImm(stackSize);
      MI->setFlag(MachineInstr::FrameSetup);
    }
  }
}

void PatmosFrameLowering::processFunctionBeforeCalleeSavedScan(
                                  MachineFunction& MF, RegScavenger* RS) const {

  const TargetInstrInfo *TII = TM.getInstrInfo();
  const TargetRegisterInfo *TRI = TM.getRegisterInfo();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  MachineFrameInfo &MFI = *MF.getFrameInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  // Insert instructions at the beginning of the entry block;
  // callee-saved-reg spills are inserted at front afterwards
  MachineBasicBlock &EntryMBB = MF.front();

  DebugLoc DL;

  // Do not emit anything for naked functions
  if (MF.getFunction()->hasFnAttribute(Attribute::Naked)) {
    return;
  }

  if (hasFP(MF)) {
    // if framepointer enabled, set it to point to the stack pointer.
    // Set frame pointer: FP = SP
    AddDefaultPred(BuildMI(EntryMBB, EntryMBB.begin(), DL,
          TII->get(Patmos::MOV), Patmos::RFP)).addReg(Patmos::RSP);
    // Mark RFP as used
    MRI.setPhysRegUsed(Patmos::RFP);
  }

  // load the current function base if it needs to be passed to call sites
  if (MFI.hasCalls()) {
    // If we have calls, we need to spill the call link registers
    MRI.setPhysRegUsed(Patmos::SRB);
    MRI.setPhysRegUsed(Patmos::SRO);
  } else {
    MRI.setPhysRegUnused(Patmos::SRB);
    MRI.setPhysRegUnused(Patmos::SRO);
  }

  // mark all predicate registers as used, for single path support
  // S0 is saved/restored as whole anyway
  if (PatmosSinglePathInfo::isEnabled(MF)) {
    MRI.setPhysRegUsed(Patmos::S0);
    MRI.setPhysRegUsed(Patmos::R26);
  }

  // If we need to spill S0, try to find an unused scratch register that we can
  // use instead. This only works if we do not have calls that may clobber
  // the register though.
  // It also makes no sense if we single-path convert the function,
  // because the SP converter introduces spill slots anyway.
  if (MRI.isPhysRegUsed(Patmos::S0) && !MF.getFrameInfo()->hasCalls()
      && !PatmosSinglePathInfo::isEnabled(MF)) {
    unsigned SpillReg = 0;
    BitVector Reserved = MRI.getReservedRegs();
    BitVector CalleeSaved(TRI->getNumRegs());
    const uint16_t *saved = TRI->getCalleeSavedRegs(&MF);
    while (*saved) {
      CalleeSaved.set(*saved++);
    }
    for (TargetRegisterClass::iterator i = Patmos::RRegsRegClass.begin(),
         e = Patmos::RRegsRegClass.end(); i != e; ++i) {
      if (MRI.isPhysRegUsed(*i) || *i == Patmos::R9) continue;
      if (Reserved[*i] || CalleeSaved[*i]) continue;
      SpillReg = *i;
      break;
    }
    if (SpillReg) {
      // Remember the register for the prologe-emitter and mark as used
      PMFI.setS0SpillReg(SpillReg);
      MRI.setPhysRegUsed(SpillReg);
    }
  }

  if (TRI->requiresRegisterScavenging(MF)) {
    const TargetRegisterClass *RC = &Patmos::RRegsRegClass;
    int fi = MFI.CreateStackObject(RC->getSize(), RC->getAlignment(), false);
    RS->addScavengingFrameIndex(fi);
    PMFI.setRegScavengingFI(fi);
  }
}

bool
PatmosFrameLowering::spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                      MachineBasicBlock::iterator MI,
                                      const std::vector<CalleeSavedInfo> &CSI,
                                      const TargetRegisterInfo *TRI) const {
  if (CSI.empty())
    return false;

  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  MachineFunction &MF = *MBB.getParent();
  const TargetInstrInfo &TII = *TM.getInstrInfo();
  PatmosMachineFunctionInfo &PMFI =
                       *MF.getInfo<PatmosMachineFunctionInfo>();

  unsigned spilledSize = 0;
  for (unsigned i = CSI.size(); i != 0; --i) {
    unsigned Reg = CSI[i-1].getReg();
    // Add the callee-saved register as live-in. It's killed at the spill.
    MBB.addLiveIn(Reg);

    // as all PRegs are aliased with S0, a spill of a Preg will cause
    // a spill of S0
    if (Patmos::PRegsRegClass.contains(Reg))
      continue;

    // Spill S0 to a register instead to a slot if there is a free register
    if (Reg == Patmos::S0 && PMFI.getS0SpillReg()) {
      TII.copyPhysReg(MBB, MI, DL, PMFI.getS0SpillReg(), Reg, true);
      prior(MI)->setFlag(MachineInstr::FrameSetup);
      continue;
    }

    // copy to R register first, then spill
    if (Patmos::SRegsRegClass.contains(Reg)) {
      TII.copyPhysReg(MBB, MI, DL, Patmos::R9, Reg, true);
      prior(MI)->setFlag(MachineInstr::FrameSetup);
      Reg = Patmos::R9;
    }

    // spill
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.storeRegToStackSlot(MBB, MI, Reg, true,
        CSI[i-1].getFrameIdx(), RC, TRI);
    prior(MI)->setFlag(MachineInstr::FrameSetup);

    // increment spilled size
    spilledSize += 4;
  }

  return true;
}

bool
PatmosFrameLowering::restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator MI,
                                        const std::vector<CalleeSavedInfo> &CSI,
                                        const TargetRegisterInfo *TRI) const {
  if (CSI.empty())
    return false;

  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  MachineFunction &MF = *MBB.getParent();
  const TargetInstrInfo &TII = *TM.getInstrInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  // if framepointer enabled, first restore the stack pointer.
  if (hasFP(MF)) {
    // Restore stack pointer: SP = FP
    AddDefaultPred(BuildMI(MBB, MI, DL, TII.get(Patmos::MOV), Patmos::RSP))
      .addReg(Patmos::RFP);
  }

  // restore the callee saved registers
  for (unsigned i = CSI.size(); i != 0; --i) {
    unsigned Reg = CSI[i-1].getReg();
    unsigned tmpReg = Reg;

    // SZ is aliased with PRegs
    if (Patmos::PRegsRegClass.contains(Reg))
        continue;

    // Spill S0 to a register instead to a slot if there is a free register
    if (Reg == Patmos::S0 && PMFI.getS0SpillReg()) {
      TII.copyPhysReg(MBB, MI, DL, Reg, PMFI.getS0SpillReg(), true);
      prior(MI)->setFlag(MachineInstr::FrameSetup);
      continue;
    }

    // copy to special register after reloading
    if (Patmos::SRegsRegClass.contains(Reg))
      tmpReg = Patmos::R9;

    // load
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(tmpReg);
    TII.loadRegFromStackSlot(MBB, MI, tmpReg, CSI[i-1].getFrameIdx(), RC, TRI);
    prior(MI)->setFlag(MachineInstr::FrameSetup);

    // copy, if needed
    if (tmpReg != Reg)
    {
      TII.copyPhysReg(MBB, MI, DL, Reg, tmpReg, true);
      prior(MI)->setFlag(MachineInstr::FrameSetup);
    }
  }

  return true;
}

void PatmosFrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  const TargetInstrInfo &TII = *TM.getInstrInfo();

  // We need to adjust the stack pointer here (and not in the prologue) to
  // handle alloca instructions that modify the stack pointer before ADJ*
  // instructions. We only need to do that if we need a frame pointer, otherwise
  // we reserve size for the call stack frame in FrameLowering in the prologue.
  if (TM.getFrameLowering()->hasFP(MF)) {
    MachineInstr &MI = *I;
    DebugLoc dl = MI.getDebugLoc();
    int Size = MI.getOperand(0).getImm();
    unsigned Opcode;
    if (MI.getOpcode() == Patmos::ADJCALLSTACKDOWN) {
      Opcode = (Size <= 0xFFF) ? Patmos::SUBi : Patmos::SUBl;
    }
    else if (MI.getOpcode() == Patmos::ADJCALLSTACKUP) {
      Opcode = (Size <= 0xFFF) ? Patmos::ADDi : Patmos::ADDl;
    }
    else {
        llvm_unreachable("Unsupported pseudo instruction.");
    }
    if (Size)
      AddDefaultPred(BuildMI(MBB, I, dl, TII.get(Opcode), Patmos::RSP))
                                                  .addReg(Patmos::RSP)
                                                  .addImm(Size);
  }
  // erase the pseudo instruction
  MBB.erase(I);
}


