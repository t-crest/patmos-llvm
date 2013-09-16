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
def FU_ALU0    : FuncUnit; // Slot 0, multiplier, branch unit, ..
def FU_ALU1    : FuncUnit; // Slot 1
def FU_MUL     : FuncUnit;

//===----------------------------------------------------------------------===//
// Patmos functional units.
//===----------------------------------------------------------------------===//
def PB_GPR     : Bypass;   // Bypass GPR registers into the EX stage
def PB_SPR     : Bypass;   // Bypass SPR registers (?)

//===----------------------------------------------------------------------===//
// Instruction Itinerary classes used for Patmos
//===----------------------------------------------------------------------===//
// generic ALU instructions
def IIC_ALUr   : InstrItinClass;
def IIC_ALUi   : InstrItinClass;
def IIC_ALUc   : InstrItinClass;
def IIC_ALUp   : InstrItinClass;
def IIC_ALUic  : InstrItinClass; // variant of ALUi for CMOV
// long immediate op
def IIC_ALUl   : InstrItinClass;
// multiply
def IIC_ALUm   : InstrItinClass;
// memory ops
def IIC_LD     : InstrItinClass; // load from global, data cache or SPM
def IIC_ST     : InstrItinClass; // store to global, data cache or SPM
def IIC_LDd    : InstrItinClass; // decoupled load
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
def IIC_CFLi   : InstrItinClass;
def IIC_CFLt   : InstrItinClass;
def IIC_CFLr   : InstrItinClass;
// pseudos
def IIC_Pseudo : InstrItinClass;

//===----------------------------------------------------------------------===//
// Patmos generic instruction itineraries.
//
// TODO Itineraries are deprecated in LLVM. This should be replaced with the 
//      new SchedReadWrite/.. model. The new model is not used by the hazard 
//      recognizer, but the hazard recognizer is only used by SelectionDAG 
//      scheduler and the default LLVM post-ra scheduler.
//===----------------------------------------------------------------------===//
// ProcessorItineraries :=  <FUs, bypasses, InstrItinData list>
//    FUs is a list of all used Function Units in this itinerary list
//    bypasses is a list of all used bypasses in this itinerary list
// InstrItinData := <InstrItinClass, InstrStage list, operand cycles, bypasses>
//    InstrStage list defines the resources used by the instruction. Only used 
//        to define the latency of the instruction itself and the resource
//        hazards.
//    operand cycles is a list of the access cycle for each operand of the 
//        Instruction (write cycle for defs, read cycle for uses)
//    bypasses is a list of bypass that is read/written for each operand of the
//        Instruction. If an instruction reads from a bypass and another 
//        instruction writes to the same bypass, the latency between those two
//        instructions is reduced by 1.
//
//    NOTE: The operand and bypass list MUST be kept in sync with the Instruction
//          operand lists in PatmosInstrInfo.td. operands and bypass define the
//          latencies *between* instructions (independent of FU list). Order of
//          operands is <defs>, <uses>, <imp-defs>, <imp-uses>.
//
// InstrStage := <cycles, FU list, optional timeinc>
//    cycles is the number of cycles needed to complete this stage, defines the
//        latency of the instruction.
//    timeinc is the number of cycles after which the next stage starts after
//        this stage has been started, defaults to "cycles". Can be used to 
//        start stages in parallel or with some delay.
//    
// We model only the execute instruction stage here, which is usually only 
// occupied for one cycle. This is only used for the hazard recognizer.
// Branch delays are not modelled here. 
//
// GPRs are read in cycle 1 (DR) (and cycle 2 from PB_GPR bypass), and are
// produced in either cycle 2 (EX) or 3 (MEM). GPRs are never written in
// the WB stage since all GPRs are forwarded. GPRs therefore have a latency 
// of either 1 or 2.
//
// Predicates are read in cycle 2 (EX) and are written in cycle 2 (EX), 
// therefore they have a latency of 1.
// NOTE: Predicate uses consist of two operands, first is the predicate, 
//       second operand is the negate flag.
// NOTE: We could model this as reading in cycle 1 (DR) and bypassing to EX,
//       but it amounts to the same thing.
//
// SPRs are read and written depending on the type of the instruction.
// NOTE: In the simulator special registers are implemented by reading in DR
//       and in effect also reading from a bypass in EX (due to the simulation
//       order of the stages). This is in effect the same as not using a bypass
//       and reading in EX, which is how we model SPRs here.
//
def PatmosGenericItineraries : ProcessorItineraries<
  [FU_ALU0, FU_ALU1, FU_MUL], [PB_GPR], [
    InstrItinData<IIC_ALUi,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ], 
                              [ 2, 2, 2, 1 ], 
			      [ PB_GPR, NoBypass, NoBypass, PB_GPR ] >,
    InstrItinData<IIC_ALUic,  [ InstrStage<1, [FU_ALU0, FU_ALU1]> ], 
			      // the old value if CMOV is not actually read
                              [ 2, 2, 2, 1, 2 ],
			      [ PB_GPR, NoBypass, NoBypass, PB_GPR, PB_GPR] >,
    InstrItinData<IIC_ALUr,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ],
                              [ 2, 2, 2, 1, 1 ], 
			      [ PB_GPR, NoBypass, NoBypass, PB_GPR, PB_GPR ] >,
    InstrItinData<IIC_ALUc,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ],
                              [ 2, 2, 2, 1, 1 ], 
			      [ PB_GPR, NoBypass, NoBypass, PB_GPR, PB_GPR ] >,
    InstrItinData<IIC_ALUp,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ],
                              [ 2, 2, 2, 2, 2, 2, 2 ] >, 
    InstrItinData<IIC_ALUl,   [ InstrStage<1, [FU_ALU0]> ],
                              [ 2, 2, 2, 1, 1 ], 
			      [ NoBypass, NoBypass, NoBypass, PB_GPR, PB_GPR ],
			      2 >,
    InstrItinData<IIC_ALUm,   // Start FU_MUL in the same cycle as the ALU, 
			      // reserve the MUL unit for 3 cycles (non-pipelined mul)
                              [ InstrStage<1, [FU_ALU0], 0>, 
                                InstrStage<3, [FU_MUL]>      ],
			      // Write results 3 cycles after EX w/o bypass
                              [ 2, 2, 1, 1, 5, 5 ],
			      [ NoBypass, NoBypass, PB_GPR, PB_GPR ] >,
    InstrItinData<IIC_LD,     [ InstrStage<1, [FU_ALU0]> ],
                              [ 3, 2, 2, 1 ], 
			      [ PB_GPR, NoBypass, NoBypass, PB_GPR ] >,
    InstrItinData<IIC_ST,     [ InstrStage<1, [FU_ALU0]> ],
                              [ 2, 2, 1, 0, 1 ], 
			      [ NoBypass, NoBypass, PB_GPR, NoBypass, PB_GPR ] >,
    InstrItinData<IIC_LDd,    [ InstrStage<1, [FU_ALU0]> ],
                              // Writing to SPR SM in MEM stage
                              [ 2, 2, 1, 0, 3 ], 
			      [ NoBypass, NoBypass, PB_GPR ] >,
    InstrItinData<IIC_LDs,    [ InstrStage<1, [FU_ALU0]> ],
                              // Using SPR SS in EX stage
                              [ 3, 2, 2, 1, 0, 2 ], 
			      [ PB_GPR, NoBypass, NoBypass, PB_GPR ] >,
    InstrItinData<IIC_STs,    [ InstrStage<1, [FU_ALU0]> ],
                              // Using SPR SS in EX stage
                              [ 2, 2, 1, 0, 1, 2 ], 
			      [ NoBypass, NoBypass, PB_GPR, NoBypass, PB_GPR ] >,
    InstrItinData<IIC_STCi,   [ InstrStage<1, [FU_ALU0]> ],
			      // write to SS/ST in MEM stage, read in EX
                              [ 2, 2, 0, 3, 3, 2, 2 ]>,
    InstrItinData<IIC_STCr,   [ InstrStage<1, [FU_ALU0]> ],
			      // write to SS/ST in MEM stage, read in EX
                              [ 2, 2, 1, 3, 3, 2, 2 ], 
			      [ NoBypass, NoBypass, PB_GPR ] >,
    InstrItinData<IIC_SPCw,   [ InstrStage<1, [FU_ALU0]> ],
                              [ 2, 2 ] >,
    InstrItinData<IIC_SPCt,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ],
			      // MTS writes to special reg in EX
                              [ 2, 2, 2, 1 ], 
			      [ NoBypass, NoBypass, NoBypass, PB_GPR ] >,
    InstrItinData<IIC_SPCf,   [ InstrStage<1, [FU_ALU0, FU_ALU1]> ],
			      // MFS read from special reg in EX
                              [ 2, 2, 2, 2 ], 
			      [ PB_GPR, NoBypass, NoBypass, NoBypass ] >,
    InstrItinData<IIC_CFLi,   [ InstrStage<1, [FU_ALU0]> ], 
                              [ 2, 2, 0 ] >,
    InstrItinData<IIC_CFLr,   [ InstrStage<1, [FU_ALU0]> ],
                              [ 2, 2, 1, 1 ], 
			      [ NoBypass, NoBypass, PB_GPR, PB_GPR ] >,
    InstrItinData<IIC_Pseudo, [ InstrStage<0, [FU_ALU0]> ]>
  ]>;

def PatmosGenericModel : SchedMachineModel {
    let IssueWidth = 2;
    let Itineraries = PatmosGenericItineraries;
    let LoadLatency = 1;
}

