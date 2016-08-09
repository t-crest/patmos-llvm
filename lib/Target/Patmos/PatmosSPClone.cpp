//===-- PatmosSPClone.cpp - Remove unused function declarations ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass clones functions on bitcode level that might be converted to
// single-path code in the Patmos backend.
// The reason for this is that a correspondence between a MachineFunction
// and "its" bitcode function needs to be maintained.
//
// As new function calls might be inserted in the lowering phase,
// all functions marked as "used" and reachable from them are cloned as well,
// and marked with the attribute "sp-maybe".
//
// The calls inserted by lowering and unnecessarily cloned functions are
// rewritten and removed, respectively, in the PatmosSPMark pass.
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
                          "because of <used> attribute");

namespace {

// Functions contained in the used array but not supported for single-path
// conversion. We do neither clone nor mark them.
const char *Blacklist[] = {"_start", "abort", "fputc", "fwrite", "setjmp"};


class PatmosSPClone : public ModulePass {
private:

  typedef std::deque<Function*> Worklist;
  typedef std::map<Function*, Function*> FunctionSPMap;

  /// Set of function names as roots
  std::set<std::string> SPRoots;

  /// Maintain a mapping from Function func to cloned Function func_sp_
  FunctionSPMap ClonedFunctions;


  void loadFromGlobalVariable(SmallSet<std::string, 128> &Result,
                              const GlobalVariable *GV) const;

  void handleRoot(Function *F);

  /**
   * Clones function F to <funcname>_sp_.
   * Adds the "sp-reachable" attribute if only_maybe==false,
   * otherwise "sp-maybe".
   */
  Function *cloneAndMark(Function *F, bool onlyMaybe=false);

  void descend(Function *F, bool fromUsed);

  /**
   * Add callees of F to worklist W and rewrite the calls.
   */
  void explore(Function *F, Worklist &W, bool fromUsed=false);

public:
  static char ID; // Pass identification, replacement for typeid

  PatmosSPClone() : ModulePass(ID) {}

  /// getPassName - Return the pass' name.
  virtual const char *getPassName() const {
    return "Patmos Single-Path Clone (bitcode)";
  }

  virtual bool doInitialization(Module &M);
  virtual bool doFinalization(Module &M);
  virtual bool runOnModule(Module &M);
};

} // end anonymous namespace

char PatmosSPClone::ID = 0;


ModulePass *llvm::createPatmosSPClonePass() {
  return new PatmosSPClone();
}

///////////////////////////////////////////////////////////////////////////////

bool PatmosSPClone::doInitialization(Module &M) {
  PatmosSinglePathInfo::getRootNames(SPRoots);
  return false;
}

bool PatmosSPClone::doFinalization(Module &M) {
  if (!SPRoots.empty()) {
    DEBUG( dbgs() << "Following single-path roots not found:\n" );
    for (std::set<std::string>::iterator it=SPRoots.begin();
            it!=SPRoots.end(); ++it) {
      DEBUG( dbgs() << "'" << *it << "' ");
    }
    DEBUG( dbgs() << '\n');
    SPRoots.clear();
  }
  return false;
}


bool PatmosSPClone::runOnModule(Module &M) {
  DEBUG( dbgs() <<
         "[Single-Path] Clone functions reachable from single-path roots\n");

  SmallSet<std::string, 128> used;
  loadFromGlobalVariable(used, M.getGlobalVariable("llvm.used"));

  SmallSet<std::string, 16> blacklst;
  blacklst.insert(Blacklist,
      Blacklist + (sizeof Blacklist / sizeof Blacklist[0]));

  //TODO in a future version of LLVM the attribute handling
  //     is likely to be different
  //AttrBuilder AB;
  //AB.addAttribute("singlepath", "root");

  for (Module::iterator I = M.begin(), E = M.end(); I != E; ) {
    Function *F = I++;

    if (F->isDeclaration()) continue;

    // handle single-path root specified by attribute
    if (F->hasFnAttribute("sp-root")) {
      handleRoot(F);
      // function might be specified also on cmdline
      (void) SPRoots.erase(F->getName());
      continue;
    }

    // handle single-path root specified on cmdline
    if (SPRoots.count(F->getName())) {
      F->addFnAttr("sp-root");
      handleRoot(F);
      (void) SPRoots.erase(F->getName());
      continue;
    }

    // Check if used; if yes, duplicate and mark as "sp-maybe".
    if (used.count(F->getName()) && !blacklst.count(F->getName())) {
      DEBUG( dbgs() << "Used: " << F->getName() << "\n" );
      descend(cloneAndMark(F, true), true);
      continue;
    }
  }
  return (NumSPRoots + NumSPReachable + NumSPUsed) > 0;
}


void PatmosSPClone::loadFromGlobalVariable(SmallSet<std::string, 128> &Result,
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


void PatmosSPClone::handleRoot(Function *F) {

  DEBUG( dbgs() << "SPRoot " << F->getName() << "\n" );
  if (!F->hasFnAttribute(llvm::Attribute::NoInline)) {
    F->addFnAttr(llvm::Attribute::NoInline);
  }
  NumSPRoots++;

  descend(F, false);
}


void PatmosSPClone::descend(Function *F, bool fromUsed) {
  // explore from root
  Worklist W;
  explore(F, W, fromUsed);
  while (!W.empty()) {
    Function *Child = W.front();
    W.pop_front();
    explore(Child, W, fromUsed);
  }
}


Function *PatmosSPClone::cloneAndMark(Function *F, bool onlyMaybe) {
  ValueToValueMapTy VMap;
  Function *SPF = CloneFunction(F, VMap, false, NULL);
  SPF->setName(F->getName() + Twine("_sp_"));

  SPF->addFnAttr(!onlyMaybe ? "sp-reachable" : "sp-maybe");
  DEBUG( dbgs() << "  Clone function: " << F->getName()
                << " -> " << SPF->getName() << "\n");
  // STATISTICS
  if (onlyMaybe) {
    NumSPUsed++;
  } else {
    NumSPReachable++;
  }
  // if the root attribute got cloned, remove it
  if (SPF->hasFnAttribute("sp-root")) {
    SPF->removeFnAttr("sp-root");
  }
  F->getParent()->getFunctionList().push_back(SPF);

  ClonedFunctions.insert(std::make_pair(F, SPF));

  return SPF;
}

void PatmosSPClone::explore(Function *F, Worklist &W, bool fromUsed) {
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
      CallInst *Call = dyn_cast<CallInst>(&*I);
      if (Call && !Call->isInlineAsm()) {
        Function *Callee = Call->getCalledFunction();

        if (!Callee) {
          // null if it is an indirect function invocation
          DEBUG( dbgs() << "  Warning: indirect function call in "
                        << F->getName() << "\n");
          continue;
        }

        // skip LLVM intrinsics
        if (Callee->isIntrinsic()) continue;

        Function *SPCallee;
        if (!ClonedFunctions.count(Callee)) {
          // clone function
          SPCallee = cloneAndMark(Callee, fromUsed);
          // add callee to worklist
          W.push_back(SPCallee);
        } else {
          // lookup
          SPCallee = ClonedFunctions.at(Callee);
        }

        // Rewrite the call to point to the cloned function
        Call->setCalledFunction(SPCallee);
      }
  }
}
