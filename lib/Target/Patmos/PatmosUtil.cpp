//==- PatmosUtil.cpp - Patmos CG utilities that fit nowhere else -*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PatmosUtil.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/Compiler.h"
#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

void getMBBIRName(const MachineBasicBlock *MBB,
                         SmallString<128> &result) {
  const BasicBlock *BB = MBB->getBasicBlock();
  const Twine bbname = (BB && BB->hasName()) ? BB->getName()
                                             : "<anonymous>";
  const Twine bb_ir_label = "#" + MBB->getParent()->getFunction()->getName() +
                            "#" + bbname +
                            "#" + Twine(MBB->getNumber());
  bb_ir_label.toVector(result);
}

std::pair<int,int> getLoopBounds(const MachineBasicBlock * MBB) {
  if(MBB && MBB->getBasicBlock()) {
    if (auto loop_bound_meta = MBB->getBasicBlock()->getTerminator()->getMetadata("llvm.loop")) {

      // We ignore the first metadata operand, as it is always a self-reference
      // in "llvm.loop".
      for(int i = 1, end = loop_bound_meta->getNumOperands(); i < end; i++) {
        auto meta_op = loop_bound_meta->getOperand(i);

        if (meta_op->getType()->isMetadataTy()) {
          auto meta_op_node = (llvm::MDNode*) meta_op;

          if( meta_op_node->getNumOperands() > 1 ){
            auto name = meta_op_node->getOperand(0);

            if(
                name->getType()->isMetadataTy() &&
                ((MDNode*)name)->getName() == "llvm.loop.bound" &&
                meta_op_node->getNumOperands() == 3
            ) {
              auto min_node = meta_op_node->getOperand(1);
              auto max_node = meta_op_node->getOperand(2);

              if(min_node->getType()->isIntegerTy() && max_node->getType()->isIntegerTy()) {
                auto min = ((llvm::ConstantInt*) min_node)->getZExtValue();
                auto max = ((llvm::ConstantInt*) max_node)->getZExtValue();

                return std::make_pair(min, max);
              } else {
                report_fatal_error(
                          "Invalid loop bound in MBB: '" +
                          MBB->getName() + "'!");
              }
            }
          }
        }
      }
    }
  }

  return std::make_pair(-1, -1);
}

} // End llvm namespace

