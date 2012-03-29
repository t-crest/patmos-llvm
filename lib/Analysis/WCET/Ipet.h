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
#include "llvm/ADT/ValueMap.h"
#include "llvm/Config/config.h"

#include "CostProvider.h"
#include "FlowFactProvider.h"

#ifdef HAVE_LPLIB_H
#include <lp_lib.h>
#else
#include <lpsolve/lp_lib.h>
#endif


using namespace llvm;

namespace wcet {

  class Ipet;
  class IpetResult;
  class IpetConfig;

  class IpetPass: public CallGraphSCCPass {
    public:
      static char ID; // Pass ID
      IpetPass(): CallGraphSCCPass(ID), ipetConfig(0), ipetResult(0) {}
      virtual ~IpetPass() { destroy(); }

      virtual bool doInitialization(CallGraph &CG);

      virtual bool runOnSCC(CallGraphSCC & SCC);

      virtual void getAnalysisUsage(AnalysisUsage & AU) const {
        AU.setPreservesAll();
        AU.addRequired<SimpleCostProvider>();
        AU.addRequired<SCEVFlowFactProvider>();
      }

      IpetResult *getIpetResult() { return ipetResult; }

      virtual void print(raw_ostream &O, const Module *M) const;

    private:
      void destroy();

      IpetConfig       *ipetConfig;
      IpetResult       *ipetResult;
  };

  class Ipet {
    public:
      Ipet(IpetConfig &config, IpetResult &result);

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

      void loadStructure(Function &F);

      lprec *initSolver(Function &F);

      void setObjective(lprec *lp, Function &F);

      void setStructConstraints(lprec *lp, Function &F);

      void setFlowConstraints(lprec *lp, Function &F);

      void finishEntry(lprec *lp, Function &F);

      bool runSolver(lprec *lp, Function &F);

      void readResults(lprec *lp, Function &F);

      void dumpProblem(lprec *lp, Function &F);

      void cleanup(lprec *lp, Function &F, bool success);

      int findEdge(const BasicBlock *source, const BasicBlock *target);

      int getConstrType(FlowFactProvider::ConstraintType type);

      IpetConfig       &config;
      IpetResult       &result;

      CostProvider     &CP;
      FlowFactProvider &FFP;

      typedef ValueMap<const BasicBlock *, size_t>   BBIndexMap;

      // Note: in the future we may change the edge and edge-type list to something more generic to support
      // context-sensitive interproc. analysis and indirect calls, so this is not the same as the Edge and EdgeList
      // from FlowFactProvider

      typedef std::pair<BasicBlock *, BasicBlock *> Edge;
      typedef std::vector<Edge> EdgeList;

      EdgeList      edges;
      BBIndexMap    bbIndexMap;
  };

  class IpetConfig {
    public:
      IpetConfig(CallGraph &CG, CostProvider &CP, FlowFactProvider &FFP) : CG(CG), CP(CP), FFP(FFP) {}

      CallGraph &getCallGraph() { return CG; }
      CostProvider &getCostProvider() { return CP; }
      FlowFactProvider &getFlowFactProvider() { return FFP; }

    private:
      CallGraph        &CG;
      CostProvider     &CP;
      FlowFactProvider &FFP;
  };

  class IpetResult {
    friend class Ipet;

    public:
      IpetResult() {}


      void reset();

      void clearResults(Function &F);

      /**
       * Get the execution frequency of a basic block for the worst-case path.
       */
      uint64_t getWCExecFrequency(const BasicBlock &BB) const;

      /**
       * get the WCET of function F (including subcalls)
       */
      uint64_t getWCET(const Function &F) const;

      bool hasWCET(const Function &F) const;

      bool inProgress(const Function &F) const;

      bool isUnbounded(const Function &F) const;

      // TODO maybe separate printing stuff (print, getBlockLabel) to separate Printer class?

      void print(raw_ostream &O) const;

      std::string getBlockLabel(const BasicBlock &BB, const Function &F) const;

    private:
      void setWCET(Function &F, uint64_t wcet);
      void setExecFrequency(const BasicBlock &BB, uint64_t freq);

      void setInProgress(const Function &F);
      void setFinished(const Function &F, bool unbounded);

      enum IpetStatus { IPET_RUNNING, IPET_UNBOUNDED };

      typedef ValueMap<const Function *,   uint64_t> FunctionMap;
      typedef ValueMap<const BasicBlock *, uint64_t> BasicBlockMap;
      typedef ValueMap<const Function *, IpetStatus> StatusMap;

      FunctionMap   costWCET;
      BasicBlockMap execFreq;
      FunctionMap   status;
  };

  // TODO IpetPrint pass: print results of Ipet pass (as graph, csv, ...)



}




#endif // _LLVM_IPET_H_
