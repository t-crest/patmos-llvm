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
#include "llvm/Support/CommandLine.h"
using namespace llvm;

static cl::opt<PrintBytesLevel> PrintBytes("fpatmos-print-bytes",
    cl::init(PrintAsEncoded),
    cl::desc("Print immediates as bytes in assembler"),
    cl::values(
      clEnumValN(PrintAsEncoded,   "none",  "Print immediates as stored"),
      clEnumValN(PrintCallAsBytes, "call",  "Print call immediates in bytes"),
      clEnumValN(PrintAllAsBytes,  "all",   "Print all immediates in bytes"),
      clEnumValEnd
    ));

static cl::opt<bool> PatmosDebugCompile("mpatmos-debug-compile",
                                        cl::desc("Compile for debugging"),
                                        cl::Hidden,
                                        cl::init(false));

PatmosMCAsmInfo::PatmosMCAsmInfo(const Target &T, StringRef TT)
 : T(T)
{
  PointerSize = 4;
  IsLittleEndian = false;
  StackGrowsUp = false;
  PCSymbol=".PC";
  SeparatorString = ";";
  CommentString = "#";
  CommentColumn = 45;
  LabelSuffix = ":";
  PrivateGlobalPrefix = ".L";
  AlignmentIsInBytes = true;
  EmitFunctionAlignment = false;
  EmitBasicBlockAlignment = false;
  MaxInstLength = 8; // for long immediates
  SupportsDebugInformation = PatmosDebugCompile;

  Data16bitsDirective = "\t.half\t";
  Data32bitsDirective = "\t.word\t";

  // Assembler dialect:
  // 0: Default syntax
  // 1: Print calls as immediates
  // 2: Print all immediates as bytes
  AssemblerDialect = PrintBytes;

  // We either need a register prefix or a global prefix
  //GlobalPrefix = ".";

}
