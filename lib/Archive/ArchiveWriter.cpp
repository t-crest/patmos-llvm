//===-- ArchiveWriter.cpp - Write LLVM archive files ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Builds up an LLVM archive file (.a) containing LLVM bitcode.
//
//===----------------------------------------------------------------------===//

#include "llvm/Bitcode/Archive.h"
#include "ArchiveInternals.h"
using namespace llvm;

// Create an empty archive.
Archive* Archive::CreateEmpty(const std::string& FilePath, LLVMContext& C) {
  Archive* result = new Archive(FilePath, C);
  return result;
}
