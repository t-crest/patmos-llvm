//==-- PatmosSinglePathInfo.cpp - Analysis Pass for SP CodeGen -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
// This file defines a pass to compute imformation for single-path converting
// seleced functions.
//
//===---------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"

#include <map>

using namespace llvm;

/// SPRootList - Option to enable single-path code generation and specify entry
///              functions. This option needs to be present even when all
///              roots are specified via attributes.
static cl::list<std::string> SPRootList(
    "mpatmos-singlepath",
    cl::value_desc("list"),
    cl::desc("Entry functions for which single-path code is generated"),
    cl::CommaSeparated);


///////////////////////////////////////////////////////////////////////////////

char PatmosSinglePathInfo::ID = 0;

/// createPatmosSinglePathInfoPass - Returns a new PatmosSinglePathInfo pass
/// \see PatmosSinglePathInfo
FunctionPass *
llvm::createPatmosSinglePathInfoPass(const PatmosTargetMachine &tm) {
  return new PatmosSinglePathInfo(tm);
}

///////////////////////////////////////////////////////////////////////////////


bool PatmosSinglePathInfo::isEnabled() {
  return !SPRootList.empty();
}

bool PatmosSinglePathInfo::isConverting(const MachineFunction &MF) {
  const PatmosMachineFunctionInfo *PMFI =
    MF.getInfo<PatmosMachineFunctionInfo>();
  return PMFI->isSinglePath();
}

bool PatmosSinglePathInfo::isEnabled(const MachineFunction &MF) {
  return isRoot(MF) || isReachable(MF) || isMaybe(MF);
}

bool PatmosSinglePathInfo::isRoot(const MachineFunction &MF) {
  return MF.getFunction()->hasFnAttribute("sp-root");
}

bool PatmosSinglePathInfo::isReachable(const MachineFunction &MF) {
  return MF.getFunction()->hasFnAttribute("sp-reachable");
}

bool PatmosSinglePathInfo::isMaybe(const MachineFunction &MF) {
  return MF.getFunction()->hasFnAttribute("sp-maybe");
}

void PatmosSinglePathInfo::getRootNames(std::set<std::string> &S) {
  S.insert( SPRootList.begin(), SPRootList.end() );
  S.erase("");
}

///////////////////////////////////////////////////////////////////////////////

PatmosSinglePathInfo::PatmosSinglePathInfo(const PatmosTargetMachine &tm)
  : MachineFunctionPass(ID), TM(tm),
    STC(tm.getSubtarget<PatmosSubtarget>()),
    TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())), Root(0) {}


bool PatmosSinglePathInfo::doInitialization(Module &M) {
  return false;
}


bool PatmosSinglePathInfo::doFinalization(Module &M) {
  if (Root) {
    delete Root;
    Root = NULL;
  }
  return false;
}

void PatmosSinglePathInfo::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<MachineLoopInfo>();
  AU.addRequired<MachineDominatorTree>();
  AU.setPreservesAll();
  MachineFunctionPass::getAnalysisUsage(AU);
}

bool PatmosSinglePathInfo::runOnMachineFunction(MachineFunction &MF) {
  if (Root) {
    delete Root;
    Root = NULL;
  }

  // only consider function actually marked for conversion
  std::string curfunc = MF.getFunction()->getName();
  if ( isConverting(MF) ) {
    DEBUG( dbgs() << "[Single-Path] Analyze '" << curfunc << "'\n" );
    analyzeFunction(MF);
  }
  // didn't modify anything
  return false;
}


void PatmosSinglePathInfo::analyzeFunction(MachineFunction &MF) {
  // we cannot handle irreducibility yet
  checkIrreducibility(MF);

  // FIXME Instead of using MachineLoopInfo for creating the Scope-tree,
  // we could use a custom algorithm (e.g. Havlak's algorithm)
  // that also checks irreducibility.
  // build the SPScope tree
  Root = SPScope::createSPScopeTree(MF, getAnalysis<MachineLoopInfo>());

  DEBUG( print(dbgs()) );

  // XXX for extensive debugging
  //MF.viewCFGOnly();
}


void PatmosSinglePathInfo::checkIrreducibility(MachineFunction &MF) const {
  // Get dominator information
  MachineDominatorTree &DT = getAnalysis<MachineDominatorTree>();

  struct BackedgeChecker {
    MachineDominatorTree &DT;
    std::set<MachineBasicBlock *> visited, finished;
    BackedgeChecker(MachineDominatorTree &dt) : DT(dt) {}
    void dfs(MachineBasicBlock *MBB) {
      visited.insert(MBB);
      for (MachineBasicBlock::const_succ_iterator si = MBB->succ_begin(),
          se = MBB->succ_end(); si != se; ++si) {
        if (!visited.count(*si)) {
          dfs(*si);
        } else if (!finished.count(*si)) {
          // visited but not finished -> this is a backedge
          // we only support natural loops, check domination
          if (!DT.dominates(*si, MBB)) {
            report_fatal_error("Single-path code generation failed due to "
                               "irreducible CFG in '" +
                               MBB->getParent()->getFunction()->getName() +
                               "'!");

          }
        }
      }
      finished.insert(MBB);
    }
  };

  BackedgeChecker c(DT);
  c.dfs(&MF.front());
}

SPScope *
PatmosSinglePathInfo::getScopeFor(const MachineBasicBlock *MBB) const {
  boost::optional<SPScope*> found = Root->findMBBScope(MBB);
  return (found.is_initialized())?
    boost::get(found) :
    NULL;
}

void PatmosSinglePathInfo::print(raw_ostream &os, const Module *M) const {
  assert(Root);
  os << "========================================\n";
  Root->dump(os);
  os << "========================================\n";
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
void PatmosSinglePathInfo::dump() const {
  print(dbgs());
}
#endif

void PatmosSinglePathInfo::walkRoot(llvm::SPScopeWalker &walker) const {
  assert( Root != NULL );
  Root->walk(walker);
}




