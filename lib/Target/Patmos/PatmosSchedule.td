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
// TODO differentiate between stack load/store, stack control, and others
def IIC_MEM    : InstrItinClass;
// special delay ops (can only be scheduled in first slot)
def IIC_SPCn   : InstrItinClass;
def IIC_SPCw   : InstrItinClass;
// control flow ops (only in first slot)
def IIC_PFL    : InstrItinClass;
// pseudos
def IIC_Pseudo : InstrItinClass;

//===----------------------------------------------------------------------===//
// Patmos generic instruction itineraries.
//===----------------------------------------------------------------------===//
def PatmosGenericItineraries : ProcessorItineraries<[], [], []>;

