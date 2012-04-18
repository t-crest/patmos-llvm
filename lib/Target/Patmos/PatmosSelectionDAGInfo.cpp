//===-- PatmosSelectionDAGInfo.cpp - Patmos SelectionDAG Info -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the PatmosSelectionDAGInfo class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-selectiondag-info"
#include "PatmosTargetMachine.h"
using namespace llvm;

PatmosSelectionDAGInfo::PatmosSelectionDAGInfo(const PatmosTargetMachine &TM)
  : TargetSelectionDAGInfo(TM) {
}

PatmosSelectionDAGInfo::~PatmosSelectionDAGInfo() {
}
