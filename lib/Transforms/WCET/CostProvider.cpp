//===- CostProvider.cpp - Simple Cost Provider Implementation -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple basic block instruction counter analysis for IPET on LLIR level.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/PassSupport.h"

#include "CostProvider.h"

using namespace llvm;


namespace wcet {



int SimpleCostProvider::getLocalCost(BasicBlock& BB) {
  cur_bb_cost = 0;
  visit(BB);
  return cur_bb_cost;
}

int SimpleCostProvider::getNonlocalCost(const CallSite &CS) {
  // TODO get from somewhere
  errs() << "** Warning: assuming nonlocal costs of 100 for call site " << CS << "\n";
  return 100;
}

int SimpleCostProvider::getNonlocalCost(const CallSite &CS, const Function &F) {
  // TODO get from somewhere
  errs() << "** Warning: assuming nonlocal costs of 100 for call site " << CS << ", calling function " << F.getName() << "\n";
  return 100;
}


} // namespace wcet

char wcet::BasicCostProvider::ID = 0;
static RegisterPass<wcet::BasicCostProvider> X("basic-cost", "Basic instruction counting cost provider");

char wcet::SimpleCostProvider::ID = 0;
static RegisterPass<wcet::SimpleCostProvider> Y("simple-cost", "Simple instruction counting cost provider");

