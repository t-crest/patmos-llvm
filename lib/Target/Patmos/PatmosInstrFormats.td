//===- PatmosInstrFormats.td - Patmos Instruction Formats-----*- tblgen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

// Format specifies some properties of the encoding used by the instruction.
// The format is stored in the TSFlags, the flags must match the table in
// MCTargetDesc/PatmosBaseInfo.h
class Format<bits<4> val> {
      bits<4> Value = val;
}

def FOther  : Format<0>;
def FPseudo : Format<1>;
def FALUi   : Format<2>;  // ALU with 12bit immediate
def FALUl   : Format<3>;  // 64bit long immediate instructions
def FALUc   : Format<4>;  // Compare of registers
def FALUp   : Format<5>;  // Combine predicates
def FALUb   : Format<6>;  // Bitcopy
def FALUci  : Format<7>;  // Compare of register and immediate
def FLDT    : Format<8>;  // Memory ops with 7bit offset
def FSTT    : Format<9>;  // Memory ops with 7bit offset
def FSTCi   : Format<10>;  // Stack control with 18bit immediate
def FSTCr   : Format<11>;  // Stack control with register
def FCFLi   : Format<12>; // Program flow control with 22bit immediate
def FCFLri  : Format<13>; // Program flow control with registers
def FCFLrs  : Format<14>; // Program flow control with registers
def FCFLrt  : Format<15>; // Program flow control with registers

//===----------------------------------------------------------------------===//
//  Describe Patmos instructions format here
//


// Generic Patmos Format
class PatmosInstBase<dag outs, dag ins, // SizeVal sz,
                 list<dag> pattern, Format form, InstrItinClass itin>
      : Instruction
{
  let Namespace = "Patmos";

  // need to initialize all (unused) bits, otherwise we get decoding conflicts
  field bits<64> Inst = 0;

  field bits<64> SoftFail = 0;

  let Size = 4;

  dag OutOperandList = outs;
  dag InOperandList  = ins;

  let Pattern     = pattern;
  let Itinerary   = itin;

  Format  Form = form;
  // Number of immediate operand, or 0 if no immediate
  field bits<4> ImmOpNo = 0;
  // Number of bits the immediate value is shifted left
  field bits<3> ImmShift = 0;
  // True if immediate is interpreted as signed
  bit           ImmSigned = 0;
  // True if this instruction may stall the CPU
  bit           mayStall = 0;

  // The layout must match the definitions in PatmosBaseInfo.h
  let TSFlags{12}   = mayStall;
  let TSFlags{11}   = ImmSigned;
  let TSFlags{10-8} = ImmShift;
  let TSFlags{7-4}  = ImmOpNo;
  let TSFlags{3-0}  = Form.Value;

  let DecoderNamespace = "Patmos";
}

class PatmosInst<dag outs, dag ins,
                 string op, string asmstr, list<dag> pattern, Format form, InstrItinClass itin>
      : PatmosInstBase<outs, ins, pattern, form, itin>
{
  // We ignore the predicate when printing, as it is output before the mnemonic
  let AsmString   = op # "\t" # asmstr;
  bits<4> p;
  let Inst{30-27} = p;

  let isPredicable = 1; // we have a fully predicated instruction set
}

// Generic instruction, not predicated (mainly for pseudo instructions)
class PatmosInstNoP<dag outs, dag ins, // SizeVal sz,
                 string op, string asmstr, list<dag> pattern, Format form, InstrItinClass itin>
      : PatmosInstBase<outs, ins, pattern, form, itin>
{
  let Inst{30-27} = 0;

  let AsmString   = op # "\t" # asmstr;
  let isPredicable = 0;
}

///////////////////////////////////////////////////////////////////////////////


// Pseudo instructions
// (note that this class is for instructions that should not be generated, not for instructions
// that are aliases for more generic instructions)
class PseudoInst<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInstNoP<outs, ins, op, asmstr, pattern, FPseudo, IIC_Pseudo>
{
  let Inst{31-0} = 0;

  let isPseudo = 1;
  let isCodeGenOnly = 1;
}


///////////////////////////////////////////////////////////////////////////////

// ALUi - Arithmetic Immediate
//
class ALUi<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern,
      InstrItinClass itin = IIC_ALUi>
  : PatmosInst<outs, ins, op, asmstr, pattern, FALUi, itin> {

  // unbound fields
  bits<5> rd;
  bits<5> rs1;
  bits<12> imm;

  let ImmOpNo  = 4;
  let ImmShift = 0;

  let Inst{26-25} = 0b00;
  let Inst{24-22} = func{2-0};
  let Inst{21-17} = rd;
  let Inst{16-12} = rs1;
  let Inst{11-0}  = imm;
}


///////////////////////////////////////////////////////////////////////////////


// ALUl - Long Immediate
//
class ALUl<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, op, asmstr, pattern, FALUl, IIC_ALUl> {

  // unbound fields
  bits<5>  rd;
  bits<5>  rs1;
  bits<32> imm;

  let Size = 8;

  let ImmOpNo   = 4;
  let ImmShift  = 0;
  // For the ALUl format, the immediate is interpreted as signed per default
  let ImmSigned = 1;

  let Inst{63} = 1;
  let Inst{62-59} = p;
  let Inst{58-54} = 0b11111;
  let Inst{53-49} = rd;
  let Inst{48-44} = rs1;
  let Inst{43-39} = 0b00000; //XXX don't care
  let Inst{38-36} = 0b000;
  let Inst{35-32} = func;
  let Inst{31-0}  = imm;

}


///////////////////////////////////////////////////////////////////////////////

// ALU - Arithmetic
// (superclass)
class ALU<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern, Format form, InstrItinClass itin>
  : PatmosInst<outs, ins, op, asmstr, pattern, form, itin> {

  // each subclass specifies Opc
  bits<3> Opc;

  // common for ALU format
  let Inst{26-22} = 0b01000;
  let Inst{6-4} = Opc;
  let Inst{3-0} = func;
}


// ALUr - Register
//
class ALUr<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern, FOther, IIC_ALUr> {

  let Opc = 0b000;

  // unbound fields
  bits<5> rd;
  bits<5> rs1;
  bits<5> rs2;

  let Inst{21-17} = rd;
  let Inst{16-12} = rs1;
  let Inst{11-7}  = rs2;
}


// ALUm - Multiply
//
class ALUm<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern, FOther, IIC_ALUm> {

  let Opc = 0b010;

  // unbound fields
  bits<5> rs1;
  bits<5> rs2;

  let Inst{16-12} = rs1;
  let Inst{11-7}  = rs2;
}

// ALUc - Compare
//
class ALUc<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern, FALUc, IIC_ALUc> {

  let Opc = 0b011;

  // unbound fields
  bits<3> pd;
  bits<5> rs1;
  bits<5> rs2;

  let Inst{19-17} = pd;
  let Inst{16-12} = rs1;
  let Inst{11-7}  = rs2;
}

// ALUci - Compare Immediate
//
class ALUci<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern, FALUci, IIC_ALUci> {

  let Opc = 0b110;

  // unbound fields
  bits<3> pd;
  bits<5> rs1;
  bits<5> imm;

  let Inst{19-17} = pd;
  let Inst{16-12} = rs1;
  let Inst{11-7}  = imm;
}

// ALUp - Predicate Combine
//
class ALUp<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern, FALUp, IIC_ALUp> {

  let Opc = 0b100;

  // unbound fields
  bits<3> pd;
  bits<4> ps1;
  bits<4> ps2;

  let Inst{19-17} = pd;
  let Inst{15-12} = ps1;
  let Inst{10-7}  = ps2;
}

// ALUb - Bitcopy
//
class ALUb<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<0, outs, ins, op, asmstr, pattern, FALUb, IIC_ALUb> {

  let Opc = 0b101;

  // unbound fields
  bits<5> rd;
  bits<5> rs1;
  bits<5> imm;
  bits<4> ps;

  let Inst{21-17} = rd;
  let Inst{16-12} = rs1;
  let Inst{11-7}  = imm;
  let Inst{3-0}   = ps;
}

///////////////////////////////////////////////////////////////////////////////



// SPC - Special
// (superclass)
class SPC<dag outs, dag ins, string op, string asmstr, list<dag> pattern, InstrItinClass itin>
  : PatmosInst<outs, ins, op, asmstr, pattern, FOther, itin> {

  // each subclass specifies Opc
  bits<3> Opc;

  // common for SPC format
  let Inst{26-22} = 0b01001;
  let Inst{6-4} = Opc;
}


// SPCt - Move To Special
//
class SPCt<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : SPC<outs, ins, op, asmstr, pattern, IIC_SPCt> {

  let Opc = 0b010;

  // unbound fields
  bits<4> sd;
  bits<5> rs;

  let Inst{16-12} = rs;
  let Inst{3-0}   = sd;
}

// SPCf - Move From Special
//
class SPCf<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : SPC<outs, ins, op, asmstr, pattern, IIC_SPCf> {

  let Opc = 0b011;

  // unbound fields
  bits<5> rd;
  bits<4> ss;

  let Inst{21-17} = rd;
  let Inst{3-0}   = ss;
}


///////////////////////////////////////////////////////////////////////////////


// LDT - Load Typed
//
class LDT<bits<3> ty, bits<1> ts1, bits<1> ts0,
      dag outs, dag ins, string op, string asmstr,
      list<dag> pattern, InstrItinClass itin>
  : PatmosInst<outs, ins, op, asmstr, pattern, FLDT, itin> {

  let mayLoad = 1;

  // unbound fields
  bits<5> rd;
  bits<5> ra;
  bits<7> imm;

  bits<5> Type;
  let Type{0} = ts0;
  let Type{1} = ts1;
  let Type{4-2} = ty;

  let ImmOpNo = 4;

  let Inst{26-22} = 0b01010;
  let Inst{21-17} = rd;
  let Inst{16-12} = ra;
  let Inst{11-7}  = Type;
  let Inst{6-0}   = imm;
}


///////////////////////////////////////////////////////////////////////////////

// STT - Store Typed
//
class STT<bits<3> ty, bits<2> td,
      dag outs, dag ins, string op, string asmstr,
      list<dag> pattern, InstrItinClass itin>
  : PatmosInst<outs, ins, op, asmstr, pattern, FSTT, itin> {


  let mayStore = 1;

  // unbound fields
  bits<5> ra;
  bits<5> rs;
  bits<7> imm;

  bits<5> Type;
  let Type{1-0} = td;
  let Type{4-2} = ty;

  let ImmOpNo = 3;

  let Inst{26-22} = 0b01011;
  let Inst{21-17} = Type;
  let Inst{16-12} = ra;
  let Inst{11-7}  = rs;
  let Inst{6-0}   = imm;
}


///////////////////////////////////////////////////////////////////////////////



// STCi - Stack Control Immediate
//
class STCi<bits<2> op,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmop, asmstr, pattern, FSTCi, IIC_STCi> {

  // unbound fields
  bits<18> imm;

  let ImmOpNo  = 2;
  let ImmShift = 2; 

  let Inst{26-22} = 0b01100;
  let Inst{21-20} = op;
  let Inst{19-18} = 0b00;
  let Inst{17-0}  = imm;
}

// STCr - Stack Control Register
//
class STCr<bits<2> op,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmop, asmstr, pattern, FSTCr, IIC_STCr> {

  // unbound fields
  bits<5> rs;

  let ImmOpNo  = 2;
  let ImmShift = 2; 

  let Inst{26-22} = 0b01100;
  let Inst{21-20} = op;
  let Inst{19-18} = 0b01;
  let Inst{16-12} = rs;
}


///////////////////////////////////////////////////////////////////////////////


// CFLi - Program Flow with immediate
//
class CFLi<bits<2> op, bits<1> d,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmop, asmstr, pattern, FCFLi, IIC_CFLi> {

  let Inst{26-25} = 0b10;
  let Inst{24-23} = op;
  let Inst{22} = d;

  bits<22> target;

  let ImmOpNo  = 2;
  let ImmShift = 2; 

  let Inst{21-0} = target;
}

// CFLr - Control Flow with registers
//
class CFLr<Format form, bits<2>regs, bits<1>d,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmop, asmstr, pattern, form, IIC_CFLr> {

  let Inst{26-23} = 0b1100;
  let Inst{22} = d;
  let Inst{3-2} = regs;
}

// CFLri - Control Flow with implicit registers
//
class CFLri<bits<2>op, bits<1>d,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : CFLr<FCFLri, 0b00, d, outs, ins, asmop, asmstr, pattern> {

  let Inst{1-0}  = op;
}

// CFLrs - Control Flow with single register
//
class CFLrs<bits<2>op, bits<1>d,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : CFLr<FCFLrs, 0b01, d, outs, ins, asmop, asmstr, pattern> {

  // unbound register fields
  bits<5> rs;

  let Inst{16-12} = rs;
  let Inst{1-0}  = op;
}

// CFLrt - Control Flow with two registers
//
class CFLrt<bits<2>op, bits<1>d,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : CFLr<FCFLrt, 0b10, d, outs, ins, asmop, asmstr, pattern> {

  // unbound register fields
  bits<5> rs1;
  bits<5> rs2;

  let Inst{16-12} = rs1;
  let Inst{11-7} = rs2;
  let Inst{1-0}  = op;
}
