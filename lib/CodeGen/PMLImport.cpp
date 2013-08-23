//===-- PMLImport.cpp -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Import PML to provide external analysis results to LLVM passes.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "pml-import"

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/CodeGen/PMLImport.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace llvm;


static cl::opt<std::string> ImportFile("mimport-pml",
   cl::desc("Read external analysis results from PML file"),
   cl::init(""));


INITIALIZE_PASS(PMLImport, "pml-import", "PML Import", false, true)

char PMLImport::ID = 0;

void PMLImport::anchor() {}

///////////////////////////////////////////////////////////////////////////////

void PMLImport::initializePass()
{
  if (ImportFile.empty()) {
    return;
  }

  OwningPtr<MemoryBuffer> Buf;
  if (MemoryBuffer::getFileOrSTDIN(ImportFile, Buf))
    return;

  yaml::Input Input(Buf->getBuffer());

  Input >> YDocs;

  if (!Input.error()) {
    Initialized = true;
  }
}

bool PMLImport::isAvailable() const {
  return false;
}

bool PMLImport::isAvailable(const MachineFunction* MF) const
{
  return false;
}

uint64_t PMLImport::getLocalWCET(const MachineBasicBlock *MBB) const
{
  return 0;
}

double PMLImport::getCriticality(const MachineBasicBlock *MBB) const
{
  // TODO get all criticalities from all contexts and target results, and take
  // the max.

  return 1.0;
}

