//===-- llvm/Target/PatmosTargetObjectFile.h - Patmos Object Info ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TARGET_PATMOS_TARGETOBJECTFILE_H
#define LLVM_TARGET_PATMOS_TARGETOBJECTFILE_H

#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"

namespace llvm {

  class PatmosTargetObjectFile : public TargetLoweringObjectFileELF {
  public:

  };
} // end namespace llvm

#endif
