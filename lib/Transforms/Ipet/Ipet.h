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

#include <lpsolve/lp_lib.h>

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
      uint64_t getWCExecFrequency(const BasicBlock &BB) const;

      /**
       * get the WCET of function F (including subcalls)
       */
      uint64_t getWCET(const Function &F) const;

      bool hasWCET(const Function &F) const;

      /**
       * Get the costs of one basic block, including nonlocal costs.
       *
       * Get the costs for the basic block from the cost analysis. Add costs of calls (use max costs
       * for indirect calls), use calculated costs for subcalls, or use (user-provided?) costs for external
       * calls.
       */
      uint64_t getCost(BasicBlock &BB);

      /**
       * Get the nonlocal costs of a call site, using either existing analysis results, a recursive analysis,
       * or the cost provider for calls to external functions.
       */
      uint64_t getNonlocalCost(CallSite &CS, Function &Callee);

      /**
       * Run (or rerun) IPET analysis on function F.
       *
       * The recursive analysis reuses analysis results for callees.
       *
       * @return true if the analysis succeeded.
       */
      bool analyze(Function &F);

      bool inProgress(const Function &F) const;

      /**
       * Run IPET analysis on a connected subgraph of the call graph.
       *
       * Can be used to perform global analyses or analyses of SCCs.
       *
       * TODO implement.
       */
      //void analyze(ArrayRef<Function> &CC);

    private:
      Function* getCallee(const CallSite &CS);

      void setWCET(Function &F, uint64_t wcet);
      void setWCExecFrequency(BasicBlock &BB, uint64_t wcef);

      void loadStructure(Function &F);

      lprec *initSolver(Function &F);

      void setObjective(lprec *lp, Function &F);

      void setStructConstraints(lprec *lp, Function &F);

      void setFlowConstraints(lprec *lp, Function &F);

      bool runSolver(lprec *lp);

      void readResults(lprec *lp, Function &F);

      void dumpProblem(lprec *lp, Function &F);

      CallGraph        &CG;
      CostProvider     &CP;
      FlowFactProvider &FFP;


  };

  // TODO IpetPrint pass: print results of Ipet pass (as graph, csv, ...)



}




#endif // _LLVM_IPET_H_
