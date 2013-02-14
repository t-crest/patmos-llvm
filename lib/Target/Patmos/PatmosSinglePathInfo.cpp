//==-- PatmosSinglePathInfo.h - Class to hold information for SP CodeGen ---==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a class to hold information and configuration of
// Single-Path Code Generation.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"

using namespace llvm;


/// SPConvList - Option to enable single-path conversion.
static cl::list<std::string> SPConvList(
  "mpatmos-spconv",
  cl::value_desc("list"),
  cl::desc("A list of functions to Single-Path convert (Patmos only)."),
  cl::CommaSeparated,
  cl::Hidden);


///////////////////////////////////////////////////////////////////////////////



PatmosSinglePathInfo::PatmosSinglePathInfo(const PatmosTargetMachine &tm)
  : TM(tm) {

  // get the set of functions to convert as specified on command line
  SPConvFuncs.insert( SPConvList.begin(), SPConvList.end() );
  DEBUG_TRACE( dbgs() << "[PatmosSP] Single-Path Info created.\n" );
}

#if 0
  if (!SPConvFuncs.empty()) {
    DEBUG( dbgs() << "Following functions to SPConv not found:\n" );
    for (std::set<std::string>::iterator it=SPConvFuncs.begin();
            it!=SPConvFuncs.end(); ++it) {
      DEBUG( dbgs() << *it << ' ');
    }
    DEBUG( dbgs() << '\n');
  }
#endif

bool PatmosSinglePathInfo::isToConvert(MachineFunction &MF) const {
  return SPConvFuncs.count(MF.getFunction()->getName()) > 0;
}





///////////////////////////////////////////////////////////////////////////////
// SPNode methods
///////////////////////////////////////////////////////////////////////////////


SPNode::SPNode(SPNode *parent, const MachineBasicBlock *header,
               const MachineBasicBlock *succ)
               : Parent(parent), SuccMBB(succ) {
  Level = 0;
  // add to parent's child list
  if (Parent) {
    Parent->Children[header] = this;
    Level = Parent->Level + 1;
  }
  // add header also to block list
  Blocks.push_back(header);
}

/// destructor - free the child nodes first, cleanup
SPNode::~SPNode() {
  for (std::map<const MachineBasicBlock*, SPNode*>::iterator
            I = Children.begin(), E = Children.end(); I != E; ++I) {
    delete I->second;
  }
  Children.clear();
}

void SPNode::addMBB(const MachineBasicBlock *MBB) {
  if (Blocks.front() != MBB) {
    Blocks.push_back(MBB);
  }
}

static void indent(unsigned level) {
  for(unsigned i=0; i<level; i++)
    dbgs() << "  ";
}

void SPNode::dump() const {
  indent(Level);
  dbgs() <<  "[BB#" << Blocks.front()->getNumber() << "]\n";
  for (unsigned i=1; i<Blocks.size(); i++) {
    indent(Level+1);
    dbgs() <<  " BB#" << Blocks[i]->getNumber() << "\n";
  }
  for (std::map<const MachineBasicBlock*, SPNode*>::const_iterator
            I = Children.begin(), E = Children.end(); I != E; ++I) {
    I->second->dump();
  }
}

