//==-- PatmosTargetMachine.h - Define TargetMachine for Patmos ---*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the Patmos specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//


#ifndef _LLVM_TARGET_PATMOS_TARGETMACHINE_H_
#define _LLVM_TARGET_PATMOS_TARGETMACHINE_H_

#include "PatmosInstrInfo.h"
#include "PatmosISelLowering.h"
#include "PatmosFrameLowering.h"
#include "PatmosSelectionDAGInfo.h"
#include "PatmosRegisterInfo.h"
#include "PatmosSubtarget.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

/// PatmosTargetMachine
///
class PatmosTargetMachine : public LLVMTargetMachine {
  PatmosSubtarget        Subtarget;
  const DataLayout       DL;       // Calculates type size & alignment
  PatmosInstrInfo        InstrInfo;
  PatmosTargetLowering   TLInfo;
  PatmosSelectionDAGInfo TSInfo;
  PatmosFrameLowering    FrameLowering;
  InstrItineraryData     InstrItins;

public:
  PatmosTargetMachine(const Target &T, StringRef TT,
                      StringRef CPU, StringRef FS,
		      TargetOptions O,
                      Reloc::Model RM, CodeModel::Model CM,
		      CodeGenOpt::Level L);

  virtual const TargetFrameLowering *getFrameLowering() const {
    return &FrameLowering;
  }

  virtual const PatmosInstrInfo *getInstrInfo() const  { return &InstrInfo; }
  virtual const DataLayout *getDataLayout() const     { return &DL;}
  virtual const PatmosSubtarget *getSubtargetImpl() const { return &Subtarget; }

  virtual const TargetRegisterInfo *getRegisterInfo() const {
    return &InstrInfo.getRegisterInfo();
  }

  virtual const PatmosTargetLowering *getTargetLowering() const {
    return &TLInfo;
  }

  virtual const PatmosSelectionDAGInfo* getSelectionDAGInfo() const {
    return &TSInfo;
  }

  virtual const InstrItineraryData *getInstrItineraryData() const {  return &InstrItins; }

  /// createPassConfig - Create a pass configuration object to be used by
  /// addPassToEmitX methods for generating a pipeline of CodeGen passes.
  virtual TargetPassConfig *createPassConfig(PassManagerBase &PM);

}; // PatmosTargetMachine.

} // end namespace llvm

#endif // _LLVM_TARGET_PATMOS_TARGETMACHINE_H_
