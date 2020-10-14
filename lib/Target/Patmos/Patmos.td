//===- Patmos.td - Describe the Patmos Target Machine ---------*- tblgen -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source 
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This is the top level entry point for the Patmos target.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"


//===----------------------------------------------------------------------===//
// Register File Description
//===----------------------------------------------------------------------===//

include "PatmosRegisterInfo.td"

//===----------------------------------------------------------------------===//
// Scheduling Descriptions
//===----------------------------------------------------------------------===//

include "PatmosSchedule.td"

//===----------------------------------------------------------------------===//
// Calling Convention Description
//===----------------------------------------------------------------------===//

include "PatmosCallingConv.td"

//===----------------------------------------------------------------------===//
// Instruction Descriptions
//===----------------------------------------------------------------------===//

include "PatmosInstrInfo.td"

def PatmosInstrInfo : InstrInfo;

def PatmosInstPrinter : AsmWriter {
  let AsmWriterClassName  = "InstPrinter";
  let isMCAsmWriter = 1;
}

//===----------------------------------------------------------------------===//
// Subtarget Features. 
//===----------------------------------------------------------------------===//

def FeatureFPU         : SubtargetFeature<"fpu", "HasFPU", "true",
                                "Implements floating point unit">;
def FeatureMethodCache : SubtargetFeature<"methodcache", "HasMethodCache", "true",
                                "Uses a method cache">;

//===----------------------------------------------------------------------===//
// Patmos supported processors.
//===----------------------------------------------------------------------===//
def : ProcessorModel<"generic", PatmosGenericModel, [FeatureMethodCache]>;

//===----------------------------------------------------------------------===//
// Target Declaration
//===----------------------------------------------------------------------===//

def Patmos : Target {
  let InstructionSet = PatmosInstrInfo;
  let AssemblyWriters = [PatmosInstPrinter];
}

