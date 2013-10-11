//===-- PatmosSPMark.cpp - Remove unused function declarations ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass loops over all of the functions in the input module, looking for
// dead declarations and removes them. Dead declarations are declarations of
// functions for which no implementation is available (i.e., declarations for
// unused library functions).
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSinglePathInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineModulePass.h"
//#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <deque>

using namespace llvm;

STATISTIC(NumSPTotal, "Total number of functions marked as single-path");
STATISTIC(NumSPMaybe, "Number of 'used' functions marked as single-path");


namespace {

/// @brief Pass to remove unused function declarations.
class PatmosSPMark : public MachineModulePass {
private:
  typedef std::deque<MachineFunction*> Worklist;

  PatmosTargetMachine &TM;

  MachineModuleInfo *MMI; // contains map Function -> MachineFunction


  void scanAndRewriteCalls(MachineFunction *MF, Worklist &W);

  const Function *getCallTarget(const MachineInstr *MI) const;

  MachineFunction *getCallTargetMF(const MachineInstr *MI) const;

  void rewriteCall(MachineInstr *MI);

public:
  static char ID; // Pass identification, replacement for typeid

  PatmosSPMark(PatmosTargetMachine &tm)
    : MachineModulePass(ID), TM(tm)
  {
    (void) TM;
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const
  {
    AU.addRequired<MachineModuleInfo>();
    AU.setPreservesAll();
    MachineModulePass::getAnalysisUsage(AU);
  }

  virtual bool doInitialization(Module &M) {
    return false;
  }

  virtual bool doFinalization(Module &M) {
    return false;
  }

  virtual bool runOnMachineModule(const Module &M);
};

} // end anonymous namespace

char PatmosSPMark::ID = 0;

ModulePass *llvm::createPatmosSPMarkPass(PatmosTargetMachine &tm) {
  return new PatmosSPMark(tm);
}

///////////////////////////////////////////////////////////////////////////////


bool PatmosSPMark::runOnMachineModule(const Module &M) {

  DEBUG( dbgs() <<
         "[Single-Path] Mark functions reachable from single-path roots\n");

  MMI = &getAnalysis<MachineModuleInfo>();
  assert(MMI);

  Worklist W;

  // initialize the worklist with machine functions that have either
  // sp-root or sp-reachable function attribute
  for(Module::const_iterator F(M.begin()), FE(M.end()); F != FE; ++F) {
    if (F->hasFnAttribute("sp-root") || F->hasFnAttribute("sp-reachable")) {
      // get the machine-level function
      MachineFunction *MF = MMI->getMachineFunction(F);
      assert( MF );
      PatmosMachineFunctionInfo *PMFI =
        MF->getInfo<PatmosMachineFunctionInfo>();
      PMFI->setSinglePath();
      NumSPTotal++; // bump STATISTIC
      W.push_back(MF);
    }
  }

  // process worklist
  while (!W.empty()) {
    MachineFunction *MF = W.front();
    W.pop_front();
    scanAndRewriteCalls(MF, W);
  }
  return true;
}

const Function *PatmosSPMark::getCallTarget(const MachineInstr *MI) const {
  if (const Function *Target =
      dyn_cast<Function>( MI->getOperand(2).getGlobal())) {
    return Target;
  }
  return NULL;
}

MachineFunction *PatmosSPMark::getCallTargetMF(const MachineInstr *MI) const {
  const Function *F = getCallTarget(MI);
  MachineFunction *MF;
  if (F && (MF = MMI->getMachineFunction(F))) {
    return MF;
  }
  return NULL;
}

void PatmosSPMark::scanAndRewriteCalls(MachineFunction *MF, Worklist &W) {
  for (MachineFunction::iterator MBB = MF->begin(), MBBE = MF->end();
                                 MBB != MBBE; ++MBB) {
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                     ME = MBB->getFirstTerminator();
                                     MI != ME; ++MI) {
      if (MI->isCall()) {
        MachineFunction *MF = getCallTargetMF(MI);
        assert(MF);
        PatmosMachineFunctionInfo *PMFI =
          MF->getInfo<PatmosMachineFunctionInfo>();
        if (!PMFI->isSinglePath()) {
          // rewrite call to _sp variant
          rewriteCall(MI);
          // set _sp MF to single path in PMFI (MF has changed!)
          MachineFunction *MF = getCallTargetMF(MI);
          PatmosMachineFunctionInfo *PMFI =
            MF->getInfo<PatmosMachineFunctionInfo>();
          // we possibly have already marked the _sp variant as single-path
          // in an earlier call
          if (!PMFI->isSinglePath()) {
            PMFI->setSinglePath();
            // add the new single-path function to the worklist
            W.push_back(MF);

            NumSPTotal++; // bump STATISTIC
            NumSPMaybe++; // bump STATISTIC
          }
        }
      }
    }
  }
}


void PatmosSPMark::rewriteCall(MachineInstr *MI) {
  // get current MBB and call target
  MachineBasicBlock *MBB = MI->getParent();
  const Function *Target = dyn_cast<Function>(
      MI->getOperand(2).getGlobal()
      );
  assert(Target->hasFnAttribute("sp-maybe"));
  // get the same function with _sp prefix
  SmallVector<char, 64> buf;
  const StringRef SPFuncName = Twine(
      Twine(Target->getName()) + Twine("_sp_")
      ).toStringRef(buf);

  const Function *SPTarget = dyn_cast<Function>(
      Target->getParent()->getNamedGlobal(SPFuncName)
      );
  assert(SPTarget && "SP-reachable function not found!");
  // Remove the call target operand and add a new target operand
  // with an MachineInstrBuilder. In this case, it is inserted at
  // the right place, before the implicit defs of the call.
  MI->RemoveOperand(2);
  MachineInstrBuilder MIB(*MBB->getParent(), MI);
  MIB.addGlobalAddress(SPTarget);
  DEBUG_TRACE( dbgs() << "    call-sp: " << *MI );
}
