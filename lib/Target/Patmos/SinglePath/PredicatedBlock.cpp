//==-- PredicatedBlock.cpp - A predicated MachineBasicBlock --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//

#include "PredicatedBlock.h"

using namespace llvm;

class PredicatedBlock::Impl {
  public:

//Fields
  /// A reference to the PredicatedBlock that uses this instance
  /// to implement its private members. (I.e. the public part
  /// of the implementation)
  PredicatedBlock &Pub;

  /// The MBB that this instance manages the predicates for.
  MachineBasicBlock &MBB;

//Constructors
  Impl(PredicatedBlock *pub, MachineBasicBlock * mbb):
    Pub(*pub), MBB(*mbb)
  {}
};

PredicatedBlock::PredicatedBlock(MachineBasicBlock *mbb):
  Priv(spimpl::make_unique_impl<Impl>(this, mbb))
{}

MachineBasicBlock *PredicatedBlock::getMBB()
{
  return &Priv->MBB;
}
