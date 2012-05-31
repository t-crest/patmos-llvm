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
  string AsmWriterClassName  = "InstPrinter";
  bit isMCAsmWriter = 1;
}

//===----------------------------------------------------------------------===//
// Subtarget Features. 
//===----------------------------------------------------------------------===//

/*
def FeatureX           : SubtargetFeature<"ext", "ExtendedInsts", "true",
                                "Enable Patmos-X extensions">;
def FeatureFPU         : SubtargetFeature<"fpu", "HasFPU", "true",
                                "Implements floating point unit">;
*/

//===----------------------------------------------------------------------===//
// Patmos supported processors.
//===----------------------------------------------------------------------===//
def : Processor<"generic", PatmosGenericItineraries, []>;

//===----------------------------------------------------------------------===//
// Target Declaration
//===----------------------------------------------------------------------===//

def Patmos : Target {
  let InstructionSet = PatmosInstrInfo;
  let AssemblyWriters = [PatmosInstPrinter];
}

