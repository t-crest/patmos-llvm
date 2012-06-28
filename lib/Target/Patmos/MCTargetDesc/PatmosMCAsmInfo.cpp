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

PatmosMCAsmInfo::PatmosMCAsmInfo(const Target &T, StringRef TT)
 : T(T)
{
  PointerSize = 4;
  IsLittleEndian = false;
  StackGrowsUp = false;
  PCSymbol=".PC";
  SeparatorString = ";;";
  CommentString = "#";
  CommentColumn = 45;
  LabelSuffix = ":";
  PrivateGlobalPrefix = ".L";
  AlignmentIsInBytes = false; // in words
  MaxInstLength = 8; // for long immediates
  SupportsDebugInformation = true;

  // 0: Default syntax, 1: alternative syntax without register prefix
  AssemblerDialect = 0;
  // We either need a register prefix or a global prefix
  //GlobalPrefix = ".";

}
