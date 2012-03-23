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

      virtual bool runOnSCC(CallGraphSCC & SCC);

      virtual void getAnalysisUsage(AnalysisUsage & AU) const {
        AU.setPreservesAll();
        AU.addRequired<BBInstCnt>();
      }

    private:
      void doIpet(Function &F);
  };
}


#endif // _LLVM_IPET_H_
