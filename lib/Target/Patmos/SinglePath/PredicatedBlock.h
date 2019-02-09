//==-- PredicatedBlock.cpp - A predicated MachineBasicBlock --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
//===---------------------------------------------------------------------===//

#ifndef TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_
#define TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_

#include "llvm/CodeGen/MachineBasicBlock.h"
#include "spimpl.h"

namespace llvm {

  /// We template PredicatedBlock such that we can use mocked MBBs when testing it.
  /// This template shouldn't be used directly outside test code, instead use 'PredicatedBlock'.
  template<class M>
  class _PredicatedBlock {
  public:

    /// Constructs a new instance where all instructions in the
    /// given MBB are predicated by the given predicate.
    _PredicatedBlock(M *mbb, unsigned predicate):
      MBB(*mbb), Pred(predicate)
    {}

    /// Get the MachineBasicBlock
    M *getMBB()
    {
      return &MBB;
    }

    /// Get the list of predicates the MBBs instructions
    /// are predicated by
    std::vector<unsigned> getBlockPredicates()
    {
      std::vector<unsigned> result;
      result.push_back(Pred);
      return result;
    }

  private:

    /// The MBB that this instance manages the predicates for.
    M &MBB;

    unsigned Pred;
  };

  /// Untemplated version of _PredicatedBlock. To be used by non-test code.
  typedef _PredicatedBlock<MachineBasicBlock> PredicatedBlock;

}

#endif /* TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_ */
