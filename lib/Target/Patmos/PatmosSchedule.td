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
def IF : FuncUnit;
def DR : FuncUnit;
def EX : FuncUnit;
def MW : FuncUnit;


def FU_ALU0 : FuncUnit;
def FU_ALU1 : FuncUnit;
def FU_MUL  : FuncUnit;


//===----------------------------------------------------------------------===//
// Instruction Itinerary classes used for Patmos
//===----------------------------------------------------------------------===//
// generic instructions
def IIC_ALU    : InstrItinClass;
// long immediate op
def IIC_ALUl   : InstrItinClass;
// multiply
def IIC_ALUm   : InstrItinClass;
// memory ops
def IIC_LD     : InstrItinClass;
def IIC_ST     : InstrItinClass;
def IIC_LDs    : InstrItinClass; // load from stack cache
def IIC_STs    : InstrItinClass; // store to stack cache
def IIC_STC    : InstrItinClass; // stack control
// special delay ops (can only be scheduled in first slot)
def IIC_SPCn   : InstrItinClass;
def IIC_SPCw   : InstrItinClass;
// control flow ops (only in first slot)
def IIC_CFL    : InstrItinClass;
// pseudos
def IIC_Pseudo : InstrItinClass;

//===----------------------------------------------------------------------===//
// Patmos generic instruction itineraries.
//===----------------------------------------------------------------------===//
// <FUs, bypasses, InstrItinData for each class>
def PatmosGenericItineraries : ProcessorItineraries<
  [FU_ALU0], [], [
    InstrItinData<IIC_ALU,    [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_ALUl,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_ALUm,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_LD,     [ InstrStage<1, [FU_ALU0]> ], [2,0]>,
    InstrItinData<IIC_ST,     [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_LDs,    [ InstrStage<1, [FU_ALU0]> ], [2,0]>,
    InstrItinData<IIC_STs,    [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_STC,    [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_SPCn,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_SPCw,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_CFL,    [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_Pseudo, [ InstrStage<1, [FU_ALU0]> ]>
  ]>;

def PatmosDualItineraries : ProcessorItineraries<
  [FU_ALU0, FU_ALU1, FU_MUL], [], [
    InstrItinData<IIC_ALU,    [ InstrStage<1, [FU_ALU0, FU_ALU1]> ]>,
    InstrItinData<IIC_ALUl,   [ InstrStage<1, [FU_ALU0], 0>,
                                InstrStage<1, [FU_ALU1]>   ]>,
    InstrItinData<IIC_ALUm,   [ InstrStage<1, [FU_ALU0], 0>,
                                InstrStage<3, [FU_MUL]>    ]>,
    InstrItinData<IIC_LD,     [ InstrStage<1, [FU_ALU0]> ], [2,0]>,
    InstrItinData<IIC_ST,     [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_LDs,    [ InstrStage<1, [FU_ALU0, FU_ALU1]> ], [2,0]>,
    InstrItinData<IIC_STs,    [ InstrStage<1, [FU_ALU0, FU_ALU1]> ]>,
    InstrItinData<IIC_STC,    [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_SPCn,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_SPCw,   [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_CFL,    [ InstrStage<1, [FU_ALU0]> ]>,
    InstrItinData<IIC_Pseudo, [ InstrStage<0, [FU_ALU0]> ]>
  ]>;
