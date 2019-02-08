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

  class PredicatedBlock {
    public:

      PredicatedBlock(MachineBasicBlock *mbb);

      MachineBasicBlock *getMBB();

    private:
      class Impl;
      /// We use the PIMPL pattern to implement the private
      /// members of this instance.
      spimpl::unique_impl_ptr<Impl> Priv;

  };

}



#endif /* TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_ */
