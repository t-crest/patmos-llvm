//===-- PatmosDFA.cpp - Infrastructure for basic data-flow analyses. ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Infrastructure to solve data-flow analysis problems on the machine-code
// DFAlevel.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-dfa"

#include "PatmosDFA.h"

// using namespace llvm;

namespace llvm {
  PatmosDFAContextProvider::PatmosDFAContextProvider(unsigned int depth) :
      MaxDepth(depth) {
  }

  void dump(const PatmosDFAContext *C) {
    dbgs() << "<";
    bool first = true;
    for(PatmosDFAContext::const_iterator i(C->begin()), ie(C->end());
        i != ie; i++) {
      if (i != C->begin())
        dbgs() << "::";
      if (*i) {
        // print caller function
        if (first) {
          MCGNode *caller = (*i)->getCaller();
          if (caller->isUnknown())
            dbgs() << "UNK::";
          else {
            dbgs() << "MF" << caller->getMF()->getFunctionNumber() << "::";
          }
          first = false;
        }

        // print call BB of call site
        const MachineInstr *MI = (*i)->getMI();
        if (!MI) {
          dbgs() << "UNK";
        }
        else {
          const MachineBasicBlock *MBB = MI->getParent();
          dbgs() << "MBB" << MBB->getNumber()
                 << "::" << "MI"
                 << std::distance(MBB->instr_begin(),
                                 MachineBasicBlock::const_instr_iterator(MI));
        }

        // print callee
        MCGNode *callee = (*i)->getCallee();
        if (callee->isUnknown())
          dbgs() << "::UNK";
        else {
          dbgs () << "::MF" << callee->getMF()->getFunctionNumber();
        }
      }
      else
        dbgs() << "()";
    }
    dbgs() << ">";
  }

  void dump(const PatmosDFALocation &L) {
    const MachineInstr *MI = L.second;
    assert(MI);
    const MachineBasicBlock *MBB = MI->getParent();
    const MachineFunction *MF = MBB->getParent();

    // print context
    dump(L.first);

    // print ID of machine basic block and, optionally, the instruction.
    dbgs() << "::MF" << MF->getFunctionNumber()
           << "::MBB" << MBB->getNumber()
           << "::" << (MI->isCall() ? "*" : "") << "MI"
           << std::distance(MBB->instr_begin(),
                           MachineBasicBlock::const_instr_iterator(L.second));
  }

  bool isLocation(const PatmosDFALocation &L, unsigned int MFID,
                  int MBBID, unsigned int MIID) {
    const MachineInstr *MI = L.second;
    assert(MI);
    const MachineBasicBlock *MBB = MI->getParent();
    const MachineFunction *MF = MBB->getParent();

    return MFID == MF->getFunctionNumber() &&
           MBBID == MBB->getNumber() &&
           MIID == std::distance(MBB->instr_begin(),
                           MachineBasicBlock::const_instr_iterator(L.second));
  }
}
