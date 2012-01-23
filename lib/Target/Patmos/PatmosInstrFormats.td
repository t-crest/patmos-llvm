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




// Format specifies the encoding used by the instruction.  This is part of the
// ad-hoc solution used to emit machine instruction encodings by our machine
// code emitter.
/*
class Format<bits<2> val> {
  bits<2> Value = val;
}

def PseudoFrm   : Format<0>;
def SingleOpFrm : Format<1>;
def DoubleOpFrm : Format<2>;
def CondJumpFrm : Format<3>;

class SourceMode<bits<2> val> {
  bits<2> Value = val;
}

def SrcReg      : SourceMode<0>;
def SrcMem      : SourceMode<1>;
def SrcIndReg   : SourceMode<2>;
def SrcPostInc  : SourceMode<3>;
def SrcImm      : SourceMode<3>;

class DestMode<bit val> {
  bit Value = val;
}

def DstReg      : DestMode<0>;
def DstMem      : DestMode<1>;

class SizeVal<bits<3> val> {
  bits<3> Value = val;
}

def SizeUnknown : SizeVal<0>; // Unknown / unset size
def SizeSpecial : SizeVal<1>; // Special instruction, e.g. pseudo
def Size2Bytes  : SizeVal<2>;
def Size4Bytes  : SizeVal<3>;
def Size6Bytes  : SizeVal<4>;
*/


///////////////////////////////////////////////////////////////////////////////


// Generic Patmos Format
class PatmosInst<dag outs, dag ins, // SizeVal sz, Format f,
                 string asmstr, list<dag> pattern> : Instruction {
  field bits<32> Inst;

  let Namespace = "Patmos";

  bits<4> pred;
  let Inst{30-27} = pred;

  dag OutOperandList = outs;
  dag InOperandList  = ins;

  //FIXME ???
  //Format Form = f;
  //SizeVal Sz = sz;

  // Define how we want to layout our TargetSpecific information field... This
  // should be kept up-to-date with the fields in the PatmosInstrInfo.h file.
  //FIXME ???
  //let TSFlags{1-0} = Form.Value;
  //let TSFlags{4-2} = Sz.Value;

  let AsmString   = asmstr;
  let Pattern     = pattern;
}



///////////////////////////////////////////////////////////////////////////////

// ALUi - Arithmetic Immediate
//
class ALUi<bits<3> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {

  let Inst{26-25} = 0b00;

  // unbound fields
  bits<5> Rd;
  bits<5> Rs1;
  bits<12> Imm12;

  bits<3> Func;
  let Func = func;

  let Inst{24-22} = Func;
  let Inst{21-17} = Rd;
  let Inst{16-12} = Rs1;
  let Inst{11-0}  = Imm12;
}


///////////////////////////////////////////////////////////////////////////////


// ALUl - Long Immediate
//
class ALUl<bits<4> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {

  let Inst{31} = 1; //FIXME

  let Inst{26-22} = 0b11111;

  // unbound fields
  bits<5>  Rd;
  bits<5>  Rs1;
  bits<32> Imm32;
  bits<4>  Func;

  let Func = func;

  let Inst{21-17} = Rd;
  let Inst{16-12} = Rs1;
  let Inst{6-4} = 0b000;
  let Inst{3-0} = Func;

  //TODO handle Imm32
}


///////////////////////////////////////////////////////////////////////////////

// ALU - Arithmetic
// (superclass)
class ALU<bits<4> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {

  // common for ALU format
  let Inst{26-22} = 0b01000;

  // each subclass specifies Opc
  bits<3> Opc;
  bits<4> Func;

  let Func = func;

  let Inst{6-4} = Opc;
  let Inst{3-0} = Func;
}


// ALUr - Register
//
class ALUr<bits<4> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, asmstr, pattern> {

  let Opc = 0b000;

  // unbound fields
  bits<5> Rd;
  bits<5> Rs1;
  bits<5> Rs2;

  let Inst{21-17} = Rd;
  let Inst{16-12} = Rs1;
  let Inst{11-7}  = Rs2;
}

// ALUu - Unary
//
class ALUu<bits<4> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, asmstr, pattern> {

  let Opc = 0b001;

  // unbound fields
  bits<5> Rd;
  bits<5> Rs1;

  let Inst{21-17} = Rd;
  let Inst{16-12} = Rs1;
}

// ALUm - Multiply
//
class ALUm<bits<4> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, asmstr, pattern> {

  let Opc = 0b010;

  // unbound fields
  bits<5> Rs1;
  bits<5> Rs2;

  let Inst{16-12} = Rs1;
  let Inst{11-7}  = Rs2;
}

// ALUc - Compare
//
class ALUc<bits<4> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, asmstr, pattern> {

  let Opc = 0b011;

  // unbound fields
  bits<3> Pd;
  bits<5> Rs1;
  bits<5> Rs2;

  let Inst{19-17} = Pd;
  let Inst{16-12} = Rs1;
  let Inst{11-7}  = Rs2;
}

// ALUp - Predicate
//
class ALUp<bits<4> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : ALU<func, outs, ins, asmstr, pattern> {

  let Opc = 0b100;

  // unbound fields
  bits<3> Pd;
  bits<4> Ps1;
  bits<4> Ps2;

  let Inst{19-17} = Pd;
  let Inst{15-12} = Ps1;
  let Inst{10-7}  = Ps2;
}


///////////////////////////////////////////////////////////////////////////////



// SPC - Special
// (superclass)
class SPC<dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {

  // common for SPC format
  let Inst{26-22} = 0b01001;

  // each subclass specifies Opc
  bits<3> Opc;
  let Inst{6-4} = Opc;
}


// SPCn - NOP
//
class SPCn<dag outs, dag ins, string asmstr, list<dag> pattern>
  : SPC<outs, ins, asmstr, pattern> {

  let Opc = 0b000;
  // unbound fields
  bits<4> Imm4;
  let Inst{3-0}  = Imm4;
}


// SPCw - Wait
//
class SPCw<bits<4> func,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : SPC<outs, ins, asmstr, pattern> {

  let Opc = 0b001;
  bits<4> Func;
  let Func = func;
  let Inst{3-0}  = Func;
}


// SPCt - Move To Special
//
class SPCt<dag outs, dag ins, string asmstr, list<dag> pattern>
  : SPC<outs, ins, asmstr, pattern> {

  let Opc = 0b010;

  // unbound fields
  bits<5> Rs1;
  bits<4> Sd;

  let Inst{16-12} = Rs1;
  let Inst{3-0}   = Sd;
}

// SPCf - Move From Special
//
class SPCf<dag outs, dag ins, string asmstr, list<dag> pattern>
  : SPC<outs, ins, asmstr, pattern> {

  let Opc = 0b011;

  // unbound fields
  bits<5> Rd;
  bits<4> Ss;

  let Inst{21-17} = Rd;
  let Inst{3-0}   = Ss;
}


///////////////////////////////////////////////////////////////////////////////


// LDT - Load Typed
//
class LDT<bits<5> ty,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {

  let Inst{26-22} = 0b01010;

  // unbound fields
  bits<5> Rd;
  bits<5> Ra;
  bits<7> Offset;

  bits<5> Type;
  let Type = ty;

  let Inst{21-17} = Rd;
  let Inst{16-12} = Ra;
  let Inst{11-7}  = Type;
  let Inst{6-0}   = Offset;
}


///////////////////////////////////////////////////////////////////////////////

// STT - Store Typed
//
class STT<bits<5> ty,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {

  let Inst{26-22} = 0b01011;

  // unbound fields
  bits<5> Ra;
  bits<5> Rs;
  bits<7> Offset;

  bits<5> Type;
  let Type = ty;

  let Inst{21-17} = Type;
  let Inst{16-12} = Ra;
  let Inst{11-7}  = Rs;
  let Inst{6-0}   = Offset;
}


///////////////////////////////////////////////////////////////////////////////



// STC - Stack Control
//
class STC<bits<2> op,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {

  let Inst{26-24} = 0b011;

  // unbound immediate
  bits<22> Imm22;

  bits<2> Op;
  let Op = op;
  let Inst{23-22} = Op;

  let Inst{21-0} = Imm22;
}


///////////////////////////////////////////////////////////////////////////////


// PFL - Program Flow
//
class PFL<dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {

  let Inst{26-25} = 0b11;

  bits<2> PFLOp;
  let Inst{23-22} = PFLOp;
}


// PFLb - Call / Branch
//
class PFLb<bits<2> pflop, dag outs, dag ins, string asmstr, list<dag> pattern>
  : PFL<outs, ins, asmstr, pattern> {

  let Inst{24} = 0;
  let PFLOp = pflop;

  bits<22> Imm22;
  let Inst{21-0} = Imm22;
}

// PFLi - Call / Branch Indirect
//
class PFLi<bits<4>op,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : PFL<outs, ins, asmstr, pattern> {

  let Inst{24} = 1;
  let PFLOp = 0b00;

  // unbound register field
  bits<5> Rs1;
  let Inst{16-12} = Rs1;

  bits<4> Op;
  let Op = op;
  let Inst{3-0} = Op;
}

// PFLr - Return
//
class PFLr<bits<4>op,
      dag outs, dag ins, string asmstr, list<dag> pattern>
  : PFL<outs, ins, asmstr, pattern> {

  let Inst{24} = 1;
  let PFLOp = 0b10;

  bits<4> Op;
  let Op = op;
  let Inst{3-0} = Op;
}


///////////////////////////////////////////////////////////////////////////////


// Pseudo instructions
//
class PseudoInst<dag outs, dag ins, string asmstr, list<dag> pattern>
  : PatmosInst<outs, ins, asmstr, pattern> {
  let Inst{31-0} = 0;
}
