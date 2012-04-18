//===-- PatmosMCAsmInfo.cpp - Patmos asm properties -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the PatmosMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "PatmosMCAsmInfo.h"
using namespace llvm;

PatmosMCAsmInfo::PatmosMCAsmInfo(const Target &T, StringRef TT) {
  PointerSize = 4;
  IsLittleEndian = true;
  StackGrowsUp = false;
  PCSymbol=".PC";
  CommentString = "#";
  LabelSuffix = ":";
  PrivateGlobalPrefix = ".L";
  AlignmentIsInBytes = false; // in words
}
