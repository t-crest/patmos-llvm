//==-- PatmosSinglePathInfo.h - Class to hold information for SP CodeGen ---==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a class to hold information and configuration of
// Single-Path Code Generation.
//
//===----------------------------------------------------------------------===//


#ifndef _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
#define _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_

#include <Patmos.h>
#include <PatmosTargetMachine.h>

#include <set>

namespace llvm {

  /// PatmosSinglePathInfo - Class to hold info about Single-path code generation
  class PatmosSinglePathInfo {
  private:

    const PatmosTargetMachine &TM;

    /// Set of functions to be converted
    std::set<std::string> SPConvFuncs;

  public:

    /// PatmosSinglePathInfo
    explicit PatmosSinglePathInfo(const PatmosTargetMachine &tm);

    bool enabled() {
      return !SPConvFuncs.empty();
    }

    /// isToConvert - Return true if the function should be if-converted
    bool isToConvert(MachineFunction &MF);

  };

} // end of namespace llvm


#endif // _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
