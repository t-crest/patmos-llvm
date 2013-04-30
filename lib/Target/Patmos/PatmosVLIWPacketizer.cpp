//===----- PatmosPacketizer.cpp - vliw packetizer ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This implements a simple VLIW packetizer using DFA. The packetizer works on
// machine basic blocks. For each instruction I in BB, the packetizer consults
// the DFA to see if machine resources are available to execute I. If so, the
// packetizer checks if I depends on any instruction J in the current packet.
// If no dependency is found, I is added to current packet and machine resource
// is marked as taken. If any dependency is found, a target API call is made to
// prune the dependence.
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "packets"
#include "llvm/CodeGen/DFAPacketizer.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/ScheduleDAG.h"
#include "llvm/CodeGen/ScheduleDAGInstrs.h"
#include "llvm/CodeGen/LatencyPriorityQueue.h"
#include "llvm/CodeGen/SchedulerRegistry.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/ScheduleHazardRecognizer.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/MC/MCInstrItineraries.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "Patmos.h"
#include "PatmosTargetMachine.h"
#include "PatmosRegisterInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosMachineFunctionInfo.h"

#include <map>

using namespace llvm;

namespace {
  class PatmosPacketizer : public MachineFunctionPass {

  public:
    static char ID;
    PatmosPacketizer() : MachineFunctionPass(ID) {}

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      AU.addRequired<MachineDominatorTree>();
      AU.addPreserved<MachineDominatorTree>();
      AU.addRequired<MachineLoopInfo>();
      AU.addPreserved<MachineLoopInfo>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    const char *getPassName() const {
      return "Patmos VLIW Packetizer";
    }

    bool runOnMachineFunction(MachineFunction &Fn);
  };
  char PatmosPacketizer::ID = 0;

  class PatmosPacketizerList : public VLIWPacketizerList {
  private:
    // Check if there is a dependence between some instruction already in this
    // packet and this instruction.
    bool Dependence;

    // Only check for dependence if there are resources available to schedule
    // this instruction.
    bool FoundSequentialDependence;

  public:
    PatmosPacketizerList(MachineFunction &MF, MachineLoopInfo &MLI,
                          MachineDominatorTree &MDT);

    // initPacketizerState - initialize some internal flags.
    virtual void initPacketizerState();

    // ignorePseudoInstruction - Ignore bundling of pseudo instructions.
    virtual bool ignorePseudoInstruction(MachineInstr *MI, MachineBasicBlock *MBB);

    // isSoloInstruction - return true if instruction MI can not be packetized
    // with any other instruction, which means that MI itself is a packet.
    virtual bool isSoloInstruction(MachineInstr *MI);

    // isLegalToPacketizeTogether - Is it legal to packetize SUI and SUJ
    // together.
    virtual bool isLegalToPacketizeTogether(SUnit *SUI, SUnit *SUJ);

    // isLegalToPruneDependencies - Is it legal to prune dependece between SUI
    // and SUJ.
    virtual bool isLegalToPruneDependencies(SUnit *SUI, SUnit *SUJ);
  };
}

bool PatmosPacketizer::runOnMachineFunction(MachineFunction &Fn) {
  const TargetInstrInfo *TII = Fn.getTarget().getInstrInfo();
  MachineLoopInfo &MLI = getAnalysis<MachineLoopInfo>();
  MachineDominatorTree &MDT = getAnalysis<MachineDominatorTree>();

  // Instantiate the packetizer.
  PatmosPacketizerList Packetizer(Fn, MLI, MDT);

  // DFA state table should not be empty.
  assert(Packetizer.getResourceTracker() && "Empty DFA table!");

  // TODO check: This is copied from the Hexagon backend. Removing KILLs might
  // be unnecessary for Patmos, it seems we only need this if we have
  // subregisters.

  //
  // Loop over all basic blocks and remove KILL pseudo-instructions
  // These instructions confuse the dependence analysis. Consider:
  // D0 = ...   (Insn 0)
  // R0 = KILL R0, D0 (Insn 1)
  // R0 = ... (Insn 2)
  // Here, Insn 1 will result in the dependence graph not emitting an output
  // dependence between Insn 0 and Insn 2. This can lead to incorrect
  // packetization
  //
  for (MachineFunction::iterator MBB = Fn.begin(), MBBe = Fn.end();
       MBB != MBBe; ++MBB) {
    MachineBasicBlock::iterator End = MBB->end();
    MachineBasicBlock::iterator MI = MBB->begin();
    while (MI != End) {
      if (MI->isKill()) {
        MachineBasicBlock::iterator DeleteMI = MI;
        ++MI;
        MBB->erase(DeleteMI);
        End = MBB->end();
        continue;
      }
      ++MI;
    }
  }

  // Loop over all of the basic blocks.
  for (MachineFunction::iterator MBB = Fn.begin(), MBBe = Fn.end();
       MBB != MBBe; ++MBB) {
    // Find scheduling regions and schedule / packetize each region.
    for(MachineBasicBlock::iterator RegionEnd = MBB->end();
        RegionEnd != MBB->begin();) {

      // The next region starts above the previous region. Look backward in the
      // instruction stream until we find the nearest boundary.
      MachineBasicBlock::iterator I = RegionEnd;
      for(;I != MBB->begin(); --I) {
        if (TII->isSchedulingBoundary(llvm::prior(I), MBB, Fn))
          break;
      }

      // Skip empty scheduling regions or regions with one instruction.
      MachineBasicBlock::iterator priorEnd = llvm::prior(RegionEnd);
      if (I == RegionEnd || I == priorEnd) {
        RegionEnd = priorEnd;
        continue;
      }

      Packetizer.PacketizeMIs(MBB, I, RegionEnd);
      RegionEnd = I;
    }
  }

  return true;
}


PatmosPacketizerList::PatmosPacketizerList(
  MachineFunction &MF, MachineLoopInfo &MLI,MachineDominatorTree &MDT)
  : VLIWPacketizerList(MF, MLI, MDT, true), Dependence(false),
    FoundSequentialDependence(false)
{
  // TODO this initializes a DefaultVLIWScheduler. Should we overwrite it?
}

void PatmosPacketizerList::initPacketizerState() {
  Dependence = false;
  FoundSequentialDependence = false;
}

// ignorePseudoInstruction - Ignore bundling of pseudo instructions.
bool PatmosPacketizerList::ignorePseudoInstruction(MachineInstr *MI,
                                                    MachineBasicBlock *MBB) {
  if (MI->isDebugValue())
    return true;

  // We must print out inline assembly
  if (MI->isInlineAsm())
    return false;

  // We check if MI has any functional units mapped to it.
  // If it doesn't, we ignore the instruction.
  const MCInstrDesc& TID = MI->getDesc();
  unsigned SchedClass = TID.getSchedClass();
  const InstrStage* IS =
                    ResourceTracker->getInstrItins()->beginStage(SchedClass);
  unsigned FuncUnits = IS->getUnits();
  return !FuncUnits;
}

// isSoloInstruction: - Returns true for instructions that must be
// scheduled in their own packet.
bool PatmosPacketizerList::isSoloInstruction(MachineInstr *MI) {

  if (MI->isInlineAsm())
    return true;

  if (MI->isEHLabel())
    return true;

  return false;
}

bool PatmosPacketizerList::isLegalToPacketizeTogether(SUnit *SUI, SUnit *SUJ) {
  MachineInstr *I = SUI->getInstr();
  MachineInstr *J = SUJ->getInstr();
  assert(I && J && "Unable to packetize null instruction!");

  const MCInstrDesc &MCIDI = I->getDesc();
  const MCInstrDesc &MCIDJ = J->getDesc();

  MachineBasicBlock::iterator II = I;

  const PatmosRegisterInfo* PRI =
                      (const PatmosRegisterInfo *) TM.getRegisterInfo();
  const PatmosInstrInfo *PII = (const PatmosInstrInfo *) TII;

  // Inline asm cannot go in the packet.
  if (I->getOpcode() == Patmos::INLINEASM)
    llvm_unreachable("Should not meet inline asm here!");

  if (isSoloInstruction(I))
    llvm_unreachable("Should not meet solo instr here!");


  return false;
}

bool PatmosPacketizerList::isLegalToPruneDependencies(SUnit *SUI, SUnit *SUJ) {
  MachineInstr *I = SUI->getInstr();
  assert(I && SUJ->getInstr() && "Unable to packetize null instruction!");

  if (Dependence) {


    return false;
  }
  return true;
}


//===----------------------------------------------------------------------===//
//                         Public Constructor Functions
//===----------------------------------------------------------------------===//

FunctionPass *llvm::createPatmosPacketizer(PatmosTargetMachine &tm) {
  return new PatmosPacketizer();
}

