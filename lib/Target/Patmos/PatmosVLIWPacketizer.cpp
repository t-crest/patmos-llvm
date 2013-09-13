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
#include "llvm/CodeGen/SchedulerRegistry.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
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
#include "PatmosInstrInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosMachineFunctionInfo.h"

#include <map>

using namespace llvm;

namespace {
  class PatmosPacketizer : public MachineFunctionPass {
  private:
    static char ID;

    PatmosTargetMachine &PTM;
  public:
    PatmosPacketizer(PatmosTargetMachine &PTM)
    : MachineFunctionPass(ID), PTM(PTM) {}

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
    const PatmosInstrInfo &PII;
  public:
    PatmosPacketizerList(PatmosTargetMachine &PTM, MachineFunction &MF,
                         MachineLoopInfo &MLI, MachineDominatorTree &MDT);

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
  };
}

bool PatmosPacketizer::runOnMachineFunction(MachineFunction &Fn) {
  const TargetInstrInfo *TII = Fn.getTarget().getInstrInfo();
  MachineLoopInfo &MLI = getAnalysis<MachineLoopInfo>();
  MachineDominatorTree &MDT = getAnalysis<MachineDominatorTree>();

  // Instantiate the packetizer.
  PatmosPacketizerList Packetizer(PTM, Fn, MLI, MDT);

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


PatmosPacketizerList::PatmosPacketizerList(PatmosTargetMachine &PTM,
  MachineFunction &MF, MachineLoopInfo &MLI,MachineDominatorTree &MDT)
  : VLIWPacketizerList(MF, MLI, MDT, true), PII(*PTM.getInstrInfo())
{
  // TODO this initializes a DefaultVLIWScheduler. Should we overwrite it?
}

void PatmosPacketizerList::initPacketizerState() {
}

// ignorePseudoInstruction - Ignore bundling of pseudo instructions.
bool PatmosPacketizerList::ignorePseudoInstruction(MachineInstr *MI,
                                                    MachineBasicBlock *MBB) {
  return PII.isPseudo(MI);
}

// isSoloInstruction: - Returns true for instructions that must be
// scheduled in their own packet.
bool PatmosPacketizerList::isSoloInstruction(MachineInstr *MI) {

  if (MI->isInlineAsm())
    return true;

  if (MI->isEHLabel() || MI->isLabel())
    return true;

  // we do not bundle nops, we assume there is a reason that they are there
  // in the first place.
  if (MI->getOpcode() == Patmos::NOP) {
    return true;
  }

  switch (getPatmosFormat(MI->getDesc().TSFlags)) {

  // 64bit instructions cannot be bundled with other instructions.
  case PatmosII::FrmALUl:
    return true;

  // all other instructions can be bundled
  default:
    return false;
  }
}

bool PatmosPacketizerList::isLegalToPacketizeTogether(SUnit *SUI, SUnit *SUJ) {
  // I is the new instruction, J is an instruction already inside the bundle
  MachineInstr *I = SUI->getInstr();
  MachineInstr *J = SUJ->getInstr();
  assert(I && J && "Unable to packetize null instruction!");

  // Note: we do not care in which slot the instruction goes. The
  // DFAPacketizer makes sure we only bundle allowed instructions, the
  // isSoloInstruction check makes sure we do not bundle calls and ALUl, and
  // the BundleSanitizer pass ensures the proper operation order.

  const PatmosInstrInfo *PII = (const PatmosInstrInfo *) TII;

  // We only have at most two instructions in a bundle, and we do not
  // allow to bundle an instruction that uses a predicate with an
  // instruction that is guarded with that predicate, so it is always safe to
  // bundle instructions with disjoint guards.
  if (PII->haveDisjointPredicates(I, J)) {
    return true;
  }

  if (SUJ->isSucc(SUI)) {
    for (unsigned i = 0; i < SUJ->Succs.size(); ++i) {

      // Iterate over all successive uses of I by J
      if (SUJ->Succs[i].getSUnit() != SUI) {
        continue;
      }

      SDep::Kind DepType = SUJ->Succs[i].getKind();

      if (DepType == SDep::Data) {
        // Cannot have data dependent instructions in same bundle.
        return false;
      }
      else if (DepType == SDep::Order) {
        // Ignore order dependence, if there is no other dependency.
      }
      else if (DepType == SDep::Output) {
        // DepReg is the register that's responsible for the dependence.
        unsigned DepReg = SUJ->Succs[i].getReg();

        // Check if I and J really defines DepReg.
        if (I->definesRegister(DepReg) || J->definesRegister(DepReg)) {
          // Do not write to the same register in one bundle, may be undefined?
          return false;
        }
      }
      // Skip over anti-dependences. Two instructions that are
      // anti-dependent can share a packet
      else if (DepType != SDep::Anti) {
        return false;
      }
    }
  }

  return true;
}


//===----------------------------------------------------------------------===//
//                         Public Constructor Functions
//===----------------------------------------------------------------------===//

FunctionPass *llvm::createPatmosPacketizer(PatmosTargetMachine &tm) {
  return new PatmosPacketizer(tm);
}

