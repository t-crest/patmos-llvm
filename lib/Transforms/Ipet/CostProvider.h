//===- CostProvider.h - Cost provider for IPET analysis -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple basic block instruction counter analysis pass for IPET on LLIR level.
//
//===----------------------------------------------------------------------===//

#ifndef _LLVM_IPET_COSTPROVIDER_H_
#define _LLVM_IPET_COSTPROVIDER_H_

//#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/BasicBlock.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/CallSite.h"
//#include "llvm/ADT/Statistic.h"
using namespace llvm;


namespace ipet {
  class CostProvider {
    public:
      virtual ~CostProvider() {}
      virtual int getLocalCost(BasicBlock& BB) = 0;

      /**
       * Get the WCET for a call site to an unknown function (should be user-provided).
       */
      virtual int getNonlocalCost(const CallSite &CS) = 0;

      /**
       * Get the WCET for a call site to a function outside this module (from stored
       * analysis results or something).
       */
      virtual int getNonlocalCost(const CallSite &CS, const Function &F) = 0;
  };

  class BasicCostProvider : public CostProvider {
    public:
      BasicCostProvider() {}
      virtual ~BasicCostProvider() {}

      virtual int getLocalCost(BasicBlock& BB) {
        return BB.size();
      }

      virtual int getNonlocalCost(const CallSite &CS) {
        return 1;
      }

      virtual int getNonlocalCost(const CallSite &CS, const Function &F) {
        return 1;
      }
  };


  class SimpleCostProvider :
    public CostProvider, public InstVisitor<SimpleCostProvider> {
    friend class InstVisitor<SimpleCostProvider>;
    //protected:
      void visitInstruction(Instruction &I) { cur_bb_cost++; }
      void visitLoadInst(LoadInst     &I) { cur_bb_cost += 20; }
      void visitStoreInst(StoreInst   &I) { cur_bb_cost += 20; }
      void visitMul(BinaryOperator    &I) { cur_bb_cost +=  4; }

      // TODO handle call, invoke and intrinsic calls


    public:
      SimpleCostProvider() : cur_bb_cost(0) {}
      virtual ~SimpleCostProvider() {}

      virtual int getLocalCost(BasicBlock& BB);

      virtual int getNonlocalCost(const CallSite &CS);

      virtual int getNonlocalCost(const CallSite &CS, const Function &F);

    private:
      int cur_bb_cost;
  };
}

#endif // _LLVM_IPET_COSTPROVIDER_H_
