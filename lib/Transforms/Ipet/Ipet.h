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
#include "llvm/Support/CallSite.h"
#include "CostProvider.h"
#include "FlowFactProvider.h"

using namespace llvm;

namespace ipet {

  class Ipet;

  class IpetPass: public CallGraphSCCPass {
    public:
      static char ID; // Pass ID
      IpetPass(): CallGraphSCCPass(ID), IPET(0), CP(0), FFP(0) {}
      virtual ~IpetPass() { destroy(); }

      virtual bool doInitialization(CallGraph &CG);

      virtual bool runOnSCC(CallGraphSCC & SCC);

      virtual void getAnalysisUsage(AnalysisUsage & AU) const {
        AU.setPreservesAll();
      }

      Ipet *getIPET() { return IPET; }

    private:
      void destroy();

      Ipet             *IPET;
      CostProvider     *CP;
      FlowFactProvider *FFP;
  };

  class Ipet {
    public:
      Ipet(CallGraph &CG, CostProvider &CP, FlowFactProvider &FFP);

      void reset();

      /**
       * Get the execution frequency of a basic block for the worst-case path.
       */
      uint64_t getWCExecFrequency(BasicBlock &BB);

      /**
       * get the WCET of function F (including subcalls)
       */
      uint64_t getWCET(Function &F);

      /**
       * Get the costs of one basic block.
       *
       * Get the costs for the basic block from the cost analysis. Add costs of calls (use max costs
       * for indirect calls), use calculated costs for subcalls, or use (user-provided?) costs for external
       * calls.
       */
      uint64_t getCost(BasicBlock &BB);

      /**
       * Run (or rerun) IPET analysis on function F.
       *
       * The recursive analysis reuses analysis results for callees.
       *
       * @return true if the analysis succeeded.
       */
      bool analyze(Function &F);

      /**
       * Run IPET analysis on a connected subgraph of the call graph.
       *
       * Can be used to perform global analyses or analyses of SCCs.
       *
       * TODO implement.
       */
      //void analyze(ArrayRef<Function> &CC);

    private:
      Function* getCallee(CallSite &CS);

      //TODO void initProblem(Function &F);

      //TODO void

      CallGraph        &CG;
      CostProvider     &CP;
      FlowFactProvider &FFP;


  };

  // TODO IpetPrint pass: print results of Ipet pass (as graph, csv, ...)



}


#endif // _LLVM_IPET_H_
