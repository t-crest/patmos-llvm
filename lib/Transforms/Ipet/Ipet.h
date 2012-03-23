//===- Ipet.cpp - Generate an ILP problem for WCET Analysis ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a pass to generate an ILP problem.
//
//===----------------------------------------------------------------------===//

#ifndef _LLVM_IPET_H_
#define _LLVM_IPET_H_

#define DEBUG_TYPE "ipet"

#include "llvm/Pass.h"
#include "llvm/CallGraphSCCPass.h"
#include "BBInstCnt.h"

using namespace llvm;

namespace ipet {

  class Ipet: public CallGraphSCCPass {
    public:
      static char ID; // Pass ID
      Ipet(): CallGraphSCCPass(ID) {}

      virtual bool doInitialization(CallGraph &CG);

      virtual bool runOnSCC(CallGraphSCC & SCC);

      virtual void getAnalysisUsage(AnalysisUsage & AU) const {
        AU.setPreservesAll();
        AU.addRequired<BBInstCnt>();
      }

      /**
       * Get the execution frequency of a basic block for the worst-case path.
       */
      // TODO int getWCExecFrequency(BasicBlock &BB);

      /**
       * get the WCET of function F (including subcalls)
       */
      // TODO int getWCET(Function &F);

      /**
       * Get the costs of one basic block.
       *
       * Get the costs for the basic block from the cost analysis. Add costs of calls (use max costs
       * for indirect calls), use calculated costs for subcalls, or use (user-provided?) costs for external
       * calls.
       *
       * @param localOnly get costs without costs of called functions.
       */
      // TODO int getCost(BasicBlock &BB, bool localOnly);

    private:
      void doIpet(Function &F);

      //TODO void initProblem(Function &F);

      //TODO void
  };

  // TODO IpetPrint pass: print results of Ipet pass (as graph, csv, ...)



}


#endif // _LLVM_IPET_H_
