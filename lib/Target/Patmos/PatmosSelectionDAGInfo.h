//===-- PatmosSelectionDAGInfo.h - Patmos SelectionDAG Info -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Patmos subclass for TargetSelectionDAGInfo.
//
//===----------------------------------------------------------------------===//

#ifndef _PATMOS_SELECTIONDAGINFO_H_
#define _PATMOS_SELECTIONDAGINFO_H_

#include "llvm/Target/TargetSelectionDAGInfo.h"

namespace llvm {

class PatmosTargetMachine;

class PatmosSelectionDAGInfo : public TargetSelectionDAGInfo {
public:
  explicit PatmosSelectionDAGInfo(const PatmosTargetMachine &TM);
  ~PatmosSelectionDAGInfo();
};

}

#endif // _PATMOS_SELECTIONDAGINFO_H_
