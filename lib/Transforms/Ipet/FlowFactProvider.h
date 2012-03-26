//===- FlowFactProvider - Flowfacts provider for IPET analysis -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple flowfact provider interface and implementation.
//
//===----------------------------------------------------------------------===//

#ifndef _LLVM_IPET_FLOWFACTPROVIDER_H_
#define _LLVM_IPET_FLOWFACTPROVIDER_H_

//#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/BasicBlock.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/ADT/StringMap.h"
//#include "llvm/ADT/Statistic.h"
using namespace llvm;


namespace ipet {

  class FlowFactProvider {
    public:
      virtual ~FlowFactProvider() {}
      virtual int getLoopBound(BasicBlock& BB) = 0;
  };

  class SimpleFlowFactProvider : public FlowFactProvider {
    public:
      SimpleFlowFactProvider() {}
      virtual ~SimpleFlowFactProvider() {}

      virtual int getLoopBound(BasicBlock& BB) {
        return 10;
      }
  };

}

#endif // _LLVM_IPET_FLOWFACTPROVIDER_H_
