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
// The reason for this is that you need to maintain a correspondence
// between a MachineFunction and "its" bitcode function.
// As function calls might be inserted in the lowering phase,
// all functions marked as "used" are cloned as well.
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

class PatmosSPClone : public ModulePass {
private:

  typedef std::deque<Function*> Worklist;
  typedef std::map<Function*, Function*> FunctionSPMap;

  /// Set of function names as roots
  std::set<std::string> SPRoots;

  FunctionSPMap FuncsReachable;


  void loadFromGlobalVariable(SmallSet<std::string, 32> &Result,
                              const GlobalVariable *GV) const;


  void handleRoot(Function *F);

  // clones function F to <funcname>_sp_ and
  // adds the "sp-reachable" attribute if reachable==true
  // (otherwise "sp-maybe")
  Function *cloneAndMark(Function *F, bool reachable=true);

  // add callees of F to worklist W and rewrite the calls
  void explore(Function *F, Worklist &W);

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


  SmallSet<std::string, 32> used;
  loadFromGlobalVariable(used, M.getGlobalVariable("llvm.used"));

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
    // The assumption is that functions called from "used" functions are
    // used themselves
    if (used.count(F->getName())) {
      //DEBUG( dbgs() << "  used: " << F->getName() << "\n" );
      (void) cloneAndMark(F, false);
      NumSPUsed++; // bump STATISTIC
      continue;
    }
  }

  // Only assign statistics if there are functions contained,
  // to adhere to the conventions.
  if (!FuncsReachable.empty()) NumSPReachable = FuncsReachable.size();

  return (NumSPRoots + NumSPReachable + NumSPUsed) > 0;
}


void PatmosSPClone::loadFromGlobalVariable(SmallSet<std::string, 32> &Result,
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

  // explore from root
  Worklist W;
  explore(F, W);
  while (!W.empty()) {
    Function *Child = W.front();
    W.pop_front();
    explore(Child, W);
  }

}


Function *PatmosSPClone::cloneAndMark(Function *F, bool reachable) {
  ValueToValueMapTy VMap;
  Function *SPF = CloneFunction(F, VMap, false, NULL);
  SPF->setName(F->getName() + Twine("_sp_"));

  SPF->addFnAttr( reachable ? "sp-reachable" : "sp-maybe");
  // if the root attribute got cloned, remove it
  if (SPF->hasFnAttribute("sp-root")) {
    SPF->removeFnAttr("sp-root");
  }
  F->getParent()->getFunctionList().push_back(SPF);
  return SPF;
}

void PatmosSPClone::explore(Function *F, Worklist &W) {
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I)
      if (CallInst *Call = dyn_cast<CallInst>(&*I)) {
        Function *Callee = Call->getCalledFunction();

        // skip LLVM intrinsics
        if (Callee->isIntrinsic()) continue;

        Function *SPCallee;
        if (!FuncsReachable.count(Callee)) {
          // clone function
          SPCallee = cloneAndMark(Callee);
          DEBUG( dbgs() << "  Clone function: " << Callee->getName()
                        << " -> " << SPCallee->getName() << "\n");
          FuncsReachable.insert(std::make_pair(Callee, SPCallee));
          // add callee to worklist
          W.push_back(SPCallee);
        } else {
          // lookup
          SPCallee = FuncsReachable.at(Callee);
        }

        // rewrite call
        Call->setCalledFunction(SPCallee);

        DEBUG( dbgs() << "  Rewrite call: " << Callee->getName()
                      << " -> " << SPCallee->getName() << "\n");
      }
}
