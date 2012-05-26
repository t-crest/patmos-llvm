//===-- PatmosTargetObjectFile.cpp - Patmos Object Files ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PatmosTargetObjectFile.h"
#include "PatmosSubtarget.h"
#include "llvm/DerivedTypes.h"
#include "llvm/GlobalVariable.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ELF.h"
using namespace llvm;


