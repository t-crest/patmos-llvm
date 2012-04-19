//===- llvm/Transforms/IPO.h - Interprocedural Transformations --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This header file defines prototypes for accessor functions that expose passes
// in the WCET transformations library.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_WCET_H
#define LLVM_TRANSFORMS_WCET_H

namespace llvm {

class ModulePass;
class FunctionPass;
class CallGraphSCCPass;

CallGraphSCCPass *createIpetPass();

FunctionPass *createDomLeavesPass();

} // End wcet namespace

#endif
