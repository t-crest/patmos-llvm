//===----- PatmosPacketizer.cpp - vliw packetizer ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass cleans up Patmos bundles by ensuring the correct order of the
// instructions in the bundles and replacing a bundle followed by a NOP with
// two unbundled instructions.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "bundlesanitizer"
#include "Patmos.h"
#include "PatmosTargetMachine.h"
#include "PatmosInstrInfo.h"
#include "PatmosSubtarget.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

namespace {
  class PatmosBundleSanitizer : public MachineFunctionPass {
  private:
    static char ID;
    const PatmosInstrInfo &PII;
  public:
    PatmosBundleSanitizer(PatmosTargetMachine &tm)
    : MachineFunctionPass(ID), PII(*tm.getInstrInfo())
    {}

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    const char *getPassName() const {
      return "Patmos Bundle Sanitizer";
    }

    bool runOnMachineFunction(MachineFunction &Fn);
  private:

    bool orderBundleOps(MachineBasicBlock &MBB,
                        MachineBasicBlock::iterator &MI);

    bool tryRemoveNOP(MachineBasicBlock *MBB, MachineBasicBlock::iterator &MI);
  };
  char PatmosBundleSanitizer::ID = 0;


bool PatmosBundleSanitizer::runOnMachineFunction(MachineFunction &Fn) {

  bool modified = false;

  // Loop over all of the basic blocks.
  for (MachineFunction::iterator MBB = Fn.begin(), MBBe = Fn.end();
       MBB != MBBe; ++MBB)
  {
    // Loop over all bundles
    for(MachineBasicBlock::iterator II = MBB->begin(), IE = MBB->end();
        II != IE; ++II)
    {
      // TODO track delay slots and latencies

      if (II->isBundle()) {
        modified |= orderBundleOps(*MBB, II);
      } else if (II->getOpcode() == Patmos::NOP) {
        modified |= tryRemoveNOP(MBB, II);
      }
    }
  }

  return modified;
}

bool PatmosBundleSanitizer::orderBundleOps(MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator &MI)
{
  MachineBasicBlock::instr_iterator II = *MI;

  ++II;
  PII.skipPseudos(MBB, II);
  if (!II->isInsideBundle()) {
    report_fatal_error("Empty bundle found.");
  }

  MachineBasicBlock::instr_iterator FirstMI = II;

  ++II;
  PII.skipPseudos(MBB, II);
  if (!II->isInsideBundle()) {
    return false;
  }

  if (!PII.canIssueInSlot(II, 1)) {
    if (!PII.canIssueInSlot(FirstMI, 1)) {
      FirstMI->dump(); II->dump();
      report_fatal_error("Bundle contains two instructions that can only be "
                         "issued in slot 0.");
    }

    // swap operations in bundle
    FirstMI->removeFromParent();
    MBB.insert(next(II), FirstMI);

    return true;
  }

  return false;
}

bool PatmosBundleSanitizer::tryRemoveNOP(MachineBasicBlock *MBB,
                                         MachineBasicBlock::iterator &MI)
{
  // TODO check if the NOP is preceded or followed by a bundle, split the bundle
  // to remove the NOP. Must take care of latencies and delay slots!!

  return false;
}

} // end namespace

//===----------------------------------------------------------------------===//
//                         Public Constructor Functions
//===----------------------------------------------------------------------===//

FunctionPass *llvm::createPatmosBundleSanitizer(PatmosTargetMachine &tm) {
  return new PatmosBundleSanitizer(tm);
}

