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
  enum PrintBytesLevel {
    PrintAsEncoded = 0, PrintCallAsBytes, PrintAllAsBytes
  };

  class PatmosMCAsmInfo : public MCAsmInfo {
    public:
      explicit PatmosMCAsmInfo(StringRef TT);

      virtual ~PatmosMCAsmInfo() {}
  };

} // namespace llvm

#endif // _PATMOS_MCASMINFO_H_
