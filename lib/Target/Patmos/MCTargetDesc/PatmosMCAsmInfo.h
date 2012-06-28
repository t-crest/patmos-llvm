//=====-- PatmosMCAsmInfo.h - Patmos asm properties -----------*- C++ -*--====//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source 
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the PatmosMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef _PATMOS_MCASMINFO_H_
#define _PATMOS_MCASMINFO_H_

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCAsmInfo.h"

namespace llvm {
  class Target;

  class PatmosMCAsmInfo : public MCAsmInfo {
    private:
      const Target &T;

    public:
      explicit PatmosMCAsmInfo(const Target &T, StringRef TT);

      // This is a nasty workaround to get Target info where we otherwise not have it
      const Target &getTarget() const { return T; }
  };

} // namespace llvm

#endif // _PATMOS_MCASMINFO_H_
