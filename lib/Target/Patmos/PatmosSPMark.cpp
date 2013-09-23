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
#include "PatmosSinglePathInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallSet.h"
//#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <deque>

using namespace llvm;

STATISTIC(NumSPRoots,     "Number of single-path roots");
STATISTIC(NumSPReachable, "Number of functions marked as single-path "
                          "reachable from roots");
STATISTIC(NumSPUsed,      "Number of functions marked as single-path "
                          "because used attribute");


namespace {

/// @brief Pass to remove unused function declarations.
class PatmosSPMark : public ModulePass {
private:

  typedef std::deque<Function*> Worklist;
  typedef SmallPtrSet<Function*, 64> FunctionSet;

  /// Set of function names as roots
  std::set<std::string> SPRoots;

  FunctionSet FuncsRoots;
  FunctionSet FuncsReachable;


  void loadFromGlobalVariable(SmallSet<std::string, 32> &Result,
                              const GlobalVariable *GV) const;


  void handleRoot(Function *F);

  // clones function F to <funcname>_sp_ and adds the "sp-reachable" attribute
  Function *cloneAndMark(Function *F);

  // add callees of F to worklist W and rewrite the calls
  void explore(Function *F, Worklist &W);

public:
  static char ID; // Pass identification, replacement for typeid

  PatmosSPMark() : ModulePass(ID) {
    initializePatmosSPMarkPass(*PassRegistry::getPassRegistry());
  }

  virtual bool doInitialization(Module &M);
  virtual bool doFinalization(Module &M);
  virtual bool runOnModule(Module &M);
};

} // end anonymous namespace

char PatmosSPMark::ID = 0;

INITIALIZE_PASS(PatmosSPMark, "patmos-spmark",
                "Mark functions for single-path conversion", false, false)


ModulePass *llvm::createPatmosSPMarkPass() {
  return new PatmosSPMark();
}

///////////////////////////////////////////////////////////////////////////////

bool PatmosSPMark::doInitialization(Module &M) {
  PatmosSinglePathInfo::getRootNames(SPRoots);
  return false;
}

bool PatmosSPMark::doFinalization(Module &M) {
  if (!SPRoots.empty()) {
    DEBUG( dbgs() << "Following single-path roots not found:\n'" );
    for (std::set<std::string>::iterator it=SPRoots.begin();
            it!=SPRoots.end(); ++it) {
      DEBUG( dbgs() << *it << "' ");
    }
    DEBUG( dbgs() << '\n');
    SPRoots.clear();
  }
  return false;
}


bool PatmosSPMark::runOnModule(Module &M) {

  DEBUG( dbgs() <<
         "[Single-Path] Mark functions reachable from single-path roots\n");


  SmallSet<std::string, 32> used;
  loadFromGlobalVariable(used, M.getGlobalVariable("llvm.used"));

  //TODO in a future version of LLVM the attribute handling
  //     is likely to be different
  //AttrBuilder AB;
  //AB.addAttribute("singlepath", "root");

  for (Module::iterator I = M.begin(), E = M.end(); I != E; ) {
    Function *F = I++;

    // check if used. if yes, duplicate and flag as reachable
    if (used.count(F->getName())) {
      (void) cloneAndMark(F);
      NumSPUsed++; // bump STATISTIC
      continue;
    }

    // handle single-path root
    if (SPRoots.count(F->getName())) {
      handleRoot(F);
      SPRoots.erase(F->getName());
    }
  }

  // Only assign statistics if there are functions contained,
  // to adhere to the conventions.
  if (!FuncsRoots.empty())     NumSPRoots     = FuncsRoots.size();
  if (!FuncsReachable.empty()) NumSPReachable = FuncsReachable.size();

  return (NumSPRoots + NumSPReachable + NumSPUsed) > 0;
}


void PatmosSPMark::loadFromGlobalVariable(SmallSet<std::string, 32> &Result,
                                          const GlobalVariable *GV) const {
  if (!GV || !GV->hasInitializer()) return;

  // Should be an array of 'i8*'.
  const ConstantArray *InitList = dyn_cast<ConstantArray>(GV->getInitializer());
  if (InitList == 0) return;

  for (unsigned i = 0, e = InitList->getNumOperands(); i != e; ++i)
    if (const Function *F =
          dyn_cast<Function>(InitList->getOperand(i)->stripPointerCasts()))
      Result.insert(F->getName());
}


void PatmosSPMark::handleRoot(Function *F) {

  DEBUG( dbgs() << "SPRoot " << F->getName() << "\n" );
  F->addFnAttr("sp-root");
  FuncsRoots.insert(F);

  // explore from root
  Worklist W;
  explore(F, W);
  while (!W.empty()) {
    Function *Child = W.front();
    W.pop_front();
    explore(Child, W);
  }

}


Function *PatmosSPMark::cloneAndMark(Function *F) {
  ValueToValueMapTy VMap;
  Function *SPF = CloneFunction(F, VMap, false, NULL);
  SPF->setName(F->getName() + Twine("_sp_"));
  SPF->addFnAttr("sp-reachable");
  F->getParent()->getFunctionList().push_back(SPF);
  return SPF;
}

void PatmosSPMark::explore(Function *F, Worklist &W) {
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I)
      if (CallInst *Call = dyn_cast<CallInst>(&*I)) {
        Function *Callee = Call->getCalledFunction();
        // if we aleady have treated the callee, skip
        if (FuncsReachable.count(Callee)) continue;

        // clone function
        Function *SPCallee = cloneAndMark(Callee);
        // rewrite call
        Call->setCalledFunction(SPCallee);
        // add callee to worklist
        W.push_back(SPCallee);
        // remember that we processed Callee
        FuncsReachable.insert(Callee);

        DEBUG( dbgs() << "  "   << Callee->getName()
                      << " -> " << SPCallee->getName() << "\n");
      }
}
