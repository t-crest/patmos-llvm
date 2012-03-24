//===- BBInstCnt.cpp - Basic block instruction counter pass ---------------===//
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

#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Statistic.h"

#include "CostProvider.h"

using namespace llvm;


namespace ipet {



int SimpleCostProvider::getCost(BasicBlock& BB) {
  cur_bb_cost = 0;
  visitBasicBlock(BB);
  return cur_bb_cost;
}




}
