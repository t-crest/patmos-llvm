//===-- RtlibDeclsAdder.cpp - Add declarations for rtlib ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass adds declarations for rtlib.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "rtlibdeclsadder"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Pass.h"
#include "llvm/Module.h"
//#include "llvm/CodeGen/IntrinsicLowering.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
using namespace llvm;

STATISTIC(NumCalls, "Number of handled calls");

//class llvm::IntrinsicLowering;

namespace {

  class RtlibDeclsAdder : public ModulePass,
                           public InstVisitor<RtlibDeclsAdder> {

  protected:
    friend class InstVisitor<RtlibDeclsAdder>;
    void visitCallInst(CallInst &CI);
    //IntrinsicLowering *IL;
    const TargetData *TD;

  public:
    static char ID; // Pass identification, replacement for typeid
    RtlibDeclsAdder() : ModulePass(ID) {
      initializeRtlibDeclsAdderPass(*PassRegistry::getPassRegistry());
    }
    virtual ~RtlibDeclsAdder() { /*delete IL;*/ }

    virtual bool runOnModule(Module &M);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      AU.addRequired<TargetData>();
    }
    virtual void print(raw_ostream &O, const Module *M) const {}

  };
}

char RtlibDeclsAdder::ID = 0;
INITIALIZE_PASS(RtlibDeclsAdder, "rtlibdeclsadder",
                "Add declarations for rtlib", false, false)

ModulePass *
llvm::createRtlibDeclsAdderPass() { return new RtlibDeclsAdder(); }

//-----------------------------------------------------------------------------

bool RtlibDeclsAdder::runOnModule(Module &M) {
  TD = &getAnalysis<TargetData>();
  //IL = new IntrinsicLowering(*TD);
  //IL->AddPrototypes(M);
  visit(M);
  //delete IL;
  return true;
}

void RtlibDeclsAdder::visitCallInst(CallInst &CI) {
  dbgs() << "Call instruction " << CI << '\n';
  if (CI.getCalledFunction()->isIntrinsic()) {
    //IL->LowerIntrinsicCall(&CI);
  }
  NumCalls++;
}



