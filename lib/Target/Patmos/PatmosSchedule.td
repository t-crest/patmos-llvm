//===-- PatmosSchedule.td - Patmos Scheduling Definitions --*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Patmos functional units.
//===----------------------------------------------------------------------===//
def FU_Decode0 : FuncUnit; // Decode 0
def FU_Decode1 : FuncUnit; // Decode 1
def FU_ALU0    : FuncUnit; // AlU 0
def FU_ALU1    : FuncUnit; // AlU 1
def FU_MUL     : FuncUnit; // Multiplier
def FU_MW0     : FuncUnit; // Memory and write-back 0
def FU_MW1     : FuncUnit; // Memory and write-back 1

//===----------------------------------------------------------------------===//
// Instruction Itinerary classes used for Patmos
//===----------------------------------------------------------------------===//
// generic ALU instructions
def IIC_ALUr   : InstrItinClass;
def IIC_ALUi   : InstrItinClass;
def IIC_ALUc   : InstrItinClass;
def IIC_ALUp   : InstrItinClass;
// long immediate op
def IIC_ALUl   : InstrItinClass;
// multiply
def IIC_ALUm   : InstrItinClass;
// memory ops
def IIC_LD     : InstrItinClass;
def IIC_ST     : InstrItinClass;
def IIC_LDs    : InstrItinClass; // load from stack cache
def IIC_STs    : InstrItinClass; // store to stack cache
// stack control
def IIC_STCi   : InstrItinClass;
def IIC_STCr   : InstrItinClass; 
// special delay ops (can only be scheduled in first slot)
def IIC_SPCw   : InstrItinClass;
def IIC_SPCt   : InstrItinClass;
def IIC_SPCf   : InstrItinClass;
// control flow ops (only in first slot)
def IIC_CFLb   : InstrItinClass;
def IIC_CFLi   : InstrItinClass;
def IIC_CFLt   : InstrItinClass;
def IIC_CFLr   : InstrItinClass;
// pseudos
def IIC_Pseudo : InstrItinClass;

//===----------------------------------------------------------------------===//
// Patmos generic instruction itineraries.
//
// TODO The itineraries are deprecated in LLVM, but the new schedule infos are 
// at the moment only used by the MI scheduler. We only model the basic ALU unit 
// here for now, to define which instructions can be scheduled in both slots. 
// The hazard recognition (delays, ..) is implemented manually and does not use the 
// itineraries.
//===----------------------------------------------------------------------===//
// ProcessorItineraries :=  <FUs, bypasses, InstrItinData list>
// InstrItinData := <InstrItinClass, InstrStage list, operand cycles, bypasses>
// InstrStage := <cycles, FU list, optional timeinc>
def PatmosGenericItineraries : ProcessorItineraries<
  [FU_ALU0, FU_ALU1], [], [
    InstrItinData<IIC_ALUr,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ]>,
    InstrItinData<IIC_ALUi,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ]>,
    InstrItinData<IIC_ALUc,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ]>,
    InstrItinData<IIC_ALUp,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ]>,
    InstrItinData<IIC_ALUl,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_ALUm,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_LD,     [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_ST,     [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_LDs,    [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_STs,    [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_STCi,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_STCr,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_SPCw,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_SPCt,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ]>,
    InstrItinData<IIC_SPCf,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ]>,
    InstrItinData<IIC_CFLb,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_CFLi,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_CFLt,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_CFLr,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_Pseudo, [ InstrStage<0, [FU_ALU0]> ]>
  ]>;

def PatmosGenericModel : SchedMachineModel {
    let IssueWidth = 2;
    let Itineraries = PatmosGenericItineraries;
    let LoadLatency = 1;
}

