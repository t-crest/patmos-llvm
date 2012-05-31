//===- PatmosInstrFormats.td - Patmos Instruction Formats-----*- tblgen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source 
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Describe Patmos instructions format here
//




// Generic Patmos Format
class PatmosInstBase<dag outs, dag ins, // SizeVal sz, Format f,
                 list<dag> pattern>
      : Instruction
{
  field bits<32> Inst;

  let Inst = 0;
  let Namespace = "Patmos";

  dag OutOperandList = outs;
  dag InOperandList  = ins;

  let Pattern     = pattern;

  // Tablegen cannot create a matcher for our format (requires starting with opcode mnemonic, not sure about bundle support)
  let isCodeGenOnly = 1;
}

class PatmosInst<dag outs, dag ins, // SizeVal sz, Format f,
                 string op, string asmstr, list<dag> pattern>
      : PatmosInstBase<outs, ins, pattern>
{
  let AsmString   = "$p " # op # " " # asmstr;
  let isPredicable = 1; // we have a fully predicated instruction set
}

// Generic instruction, not predicated (mainly for pseudo instructions)
class PatmosInstNoP<dag outs, dag ins, // SizeVal sz, Format f,
                 string op, string asmstr, list<dag> pattern>
      : PatmosInstBase<outs, ins, pattern>
{
  let Inst{30-27} = 0;

  let AsmString   = op # " " # asmstr;
  let isPredicable = 0; // we have a fully predicated instruction set
}

///////////////////////////////////////////////////////////////////////////////


// Pseudo instructions
//
class PseudoInst<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInstNoP<outs, ins, op, asmstr, pattern> 
{
  let Inst{31-0} = 0;

  let isCodeGenOnly = 1;
}


///////////////////////////////////////////////////////////////////////////////

// ALUi - Arithmetic Immediate
//
class ALUi<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, op, asmstr, pattern> {

  // unbound fields
  bits<5> Rd;
  bits<4> Pred;
  bits<5> Rs1;
  bits<12> Imm12;

  let Inst{30-27} = Pred;
  let Inst{26-25} = 0b00;
  let Inst{24-22} = func{2-0};
  let Inst{21-17} = Rd;
  let Inst{16-12} = Rs1;
  let Inst{11-0}  = Imm12;
}


///////////////////////////////////////////////////////////////////////////////


// ALUl - Long Immediate
//
class ALUl<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, op, asmstr, pattern> {

  // unbound fields
  bits<5>  Rd;
  bits<4>  Pred;
  bits<5>  Rs1;
  bits<32> Imm32;

  let Inst{31} = 1; //FIXME?
  let Inst{30-27} = Pred;
  let Inst{26-22} = 0b11111;
  let Inst{21-17} = Rd;
  let Inst{16-12} = Rs1;
  let Inst{11-7} = 0b00000; //XXX don't care
  let Inst{6-4} = 0b000;
  let Inst{3-0} = func;

  //TODO handle Imm32
}


///////////////////////////////////////////////////////////////////////////////

// ALU - Arithmetic
// (superclass)
class ALU<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, op, asmstr, pattern> {

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
  : ALU<func, outs, ins, op, asmstr, pattern> {

  let Opc = 0b000;

  // unbound fields
  bits<5> Rd;
  bits<4> Pred;
  bits<5> Rs1;
  bits<5> Rs2;

  let Inst{30-27} = Pred;
  let Inst{21-17} = Rd;
  let Inst{16-12} = Rs1;
  let Inst{11-7}  = Rs2;
}

// ALUu - Unary
//
class ALUu<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern> {

  let Opc = 0b001;

  // unbound fields
  bits<5> Rd;
  bits<4> Pred;
  bits<5> Rs1;

  let Inst{30-27} = Pred;
  let Inst{21-17} = Rd;
  let Inst{16-12} = Rs1;
}

// ALUm - Multiply
//
class ALUm<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern> {

  let Opc = 0b010;

  // unbound fields
  bits<4> Pred;
  bits<5> Rs1;
  bits<5> Rs2;

  let Inst{30-27} = Pred;
  let Inst{16-12} = Rs1;
  let Inst{11-7}  = Rs2;
}

// ALUc - Compare
//
class ALUc<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern> {

  let Opc = 0b011;

  // unbound fields
  bits<3> Pd;
  bits<4> Pred;
  bits<5> Rs1;
  bits<5> Rs2;

  let Inst{30-27} = Pred;
  let Inst{19-17} = Pd;
  let Inst{16-12} = Rs1;
  let Inst{11-7}  = Rs2;
}

// ALUp - Predicate
//
class ALUp<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, op, asmstr, pattern> {

  let Opc = 0b100;

  // unbound fields
  bits<3> Pd;
  bits<4> Pred;
  bits<4> Ps1;
  bits<4> Ps2;

  let Inst{30-27} = Pred;
  let Inst{19-17} = Pd;
  let Inst{15-12} = Ps1;
  let Inst{10-7}  = Ps2;
}


///////////////////////////////////////////////////////////////////////////////



// SPC - Special
// (superclass)
class SPC<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, op, asmstr, pattern> {

  // each subclass specifies Opc
  bits<3> Opc;

  // common for SPC format
  let Inst{26-22} = 0b01001;
  let Inst{6-4} = Opc;
}


// SPCn - NOP
//
class SPCn<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : SPC<outs, ins, op, asmstr, pattern> {

  let Opc = 0b000;

  // unbound fields
  bits<4> Pred;
  bits<4> Imm4;

  let Inst{30-27} = Pred;
  let Inst{3-0}  = Imm4;
}


// SPCw - Wait
//
class SPCw<bits<4> func,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : SPC<outs, ins, op, asmstr, pattern> {

  let Opc = 0b001;

  // unbound fields
  bits<4> Pred;

  let Inst{30-27} = Pred;
  let Inst{3-0}   = func;
}


// SPCt - Move To Special
//
class SPCt<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : SPC<outs, ins, op, asmstr, pattern> {

  let Opc = 0b010;

  // unbound fields
  bits<4> Sd;
  bits<4> Pred;
  bits<5> Rs1;

  let Inst{30-27} = Pred;
  let Inst{16-12} = Rs1;
  let Inst{3-0}   = Sd;
}

// SPCf - Move From Special
//
class SPCf<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : SPC<outs, ins, op, asmstr, pattern> {

  let Opc = 0b011;

  // unbound fields
  bits<5> Rd;
  bits<4> Pred;
  bits<4> Ss;

  let Inst{30-27} = Pred;
  let Inst{21-17} = Rd;
  let Inst{3-0}   = Ss;
}


///////////////////////////////////////////////////////////////////////////////


// LDT - Load Typed
//
class LDT<bits<3> ty, bits<1> ts1, bits<1> ts0,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, op, asmstr, pattern> {

  // unbound fields
  bits<5> Rd;
  bits<4> Pred;
  bits<5> Ra;
  bits<7> Offset;

  bits<5> Type;
  let Type{0} = ts0;
  let Type{1} = ts1;
  let Type{4-2} = ty;

  let Inst{30-27} = Pred;
  let Inst{26-22} = 0b01010;
  let Inst{21-17} = Rd;
  let Inst{16-12} = Ra;
  let Inst{11-7}  = Type;
  let Inst{6-0}   = Offset;
}


///////////////////////////////////////////////////////////////////////////////

// STT - Store Typed
//
class STT<bits<3> ty, bits<2> td,
      dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, op, asmstr, pattern> {


  // unbound fields
  bits<4> Pred;
  bits<5> Ra;
  bits<5> Rs;
  bits<7> Offset;

  bits<5> Type;
  let Type{1-0} = td;
  let Type{4-2} = ty;

  let Inst{30-27} = Pred;
  let Inst{26-22} = 0b01011;
  let Inst{21-17} = Type;
  let Inst{16-12} = Ra;
  let Inst{11-7}  = Rs;
  let Inst{6-0}   = Offset;
}


///////////////////////////////////////////////////////////////////////////////



// STC - Stack Control
//
class STC<bits<2> op,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmop, asmstr, pattern> {

  // unbound fields
  bits<4> Pred;
  bits<22> Imm22;

  let Inst{30-27} = Pred;
  let Inst{26-24} = 0b011;
  let Inst{23-22} = op;
  let Inst{21-0} = Imm22;
}


///////////////////////////////////////////////////////////////////////////////


// PFL - Program Flow
//
class PFL<dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, op, asmstr, pattern> {

  bits<2> PFLOp;
  let Inst{26-25} = 0b11;
  let Inst{23-22} = PFLOp;
}


// PFLb - Call / Branch
//
class PFLb<bits<2> pflop, dag outs, dag ins, string op, string asmstr, list<dag> pattern>
  : PFL<outs, ins, op, asmstr, pattern> {

  let PFLOp = pflop;

  bits<4> Pred;
  bits<22> Imm22;
  let Inst{30-27} = Pred;
  let Inst{24} = 0;
  let Inst{21-0} = Imm22;
}

// PFLi - Call / Branch Indirect
//
class PFLi<bits<4>op,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : PFL<outs, ins, asmop, asmstr, pattern> {

  let PFLOp = 0b00;

  // unbound register field
  bits<4> Pred;
  bits<5> Rs1;

  let Inst{30-27} = Pred;
  let Inst{24} = 1;
  let Inst{16-12} = Rs1;
  let Inst{3-0} = op;
}

// PFLr - Return
//
class PFLr<bits<4>op,
      dag outs, dag ins, string asmop, string asmstr, list<dag> pattern>
  : PFL<outs, ins, asmop, asmstr, pattern> {

  let PFLOp = 0b10;

  bits<4> Pred;

  let Inst{30-27} = Pred;
  let Inst{24} = 1;
  let Inst{3-0} = op;
}


