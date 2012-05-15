//===- PatmosInstrInfo.td - Patmos Instruction defs -----------*- tblgen-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source 
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes the Patmos instructions in TableGen format.
//
//===----------------------------------------------------------------------===//

include "PatmosInstrFormats.td"

//===----------------------------------------------------------------------===//
// Type Constraints.
//===----------------------------------------------------------------------===//
class SDTCisI8<int OpNum>  : SDTCisVT<OpNum,  i8>;
class SDTCisI16<int OpNum> : SDTCisVT<OpNum, i16>;
class SDTCisI32<int OpNum> : SDTCisVT<OpNum, i32>;

//===----------------------------------------------------------------------===//
// Type Profiles.
//===----------------------------------------------------------------------===//

//def SDT_PatmosCall         : SDTypeProfile<0, -1, [SDTCisVT<0, iPTR>]>;
def SDT_PatmosCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>]>;
def SDT_PatmosCallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;

//===----------------------------------------------------------------------===//
// Patmos Specific Node Definitions.
//===----------------------------------------------------------------------===//

def retflag       : SDNode<"PatmosISD::RET_FLAG", SDTNone,
                          [SDNPHasChain, SDNPOptInGlue]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_PatmosCallSeqStart,
                          [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_PatmosCallSeqEnd,
                          [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def patmos_mul    : SDNode<"PatmosISD::MUL", SDTIntBinOp,
                          [SDNPCommutative, SDNPAssociative]>;


//===----------------------------------------------------------------------===//
// Patmos Operand Definitions.
//===----------------------------------------------------------------------===//



/*
// Address operands
def memsrc : Operand<i16> {
  let PrintMethod = "printSrcMemOperand";
  let MIOperandInfo = (ops GR16, i16imm);
}

def memdst : Operand<i16> {
  let PrintMethod = "printSrcMemOperand";
  let MIOperandInfo = (ops GR16, i16imm);
}

// Short jump targets have OtherVT type and are printed as pcrel imm values.
def jmptarget : Operand<OtherVT> {
  let PrintMethod = "printPCRelImmOperand";
}

// Operand for printing out a condition code.
def cc : Operand<i8> {
  let PrintMethod = "printCCOperand";
}
*/


//===----------------------------------------------------------------------===//
// Transformation Functions
//===----------------------------------------------------------------------===//

// Transformation Function - shift right by one/two bits
def shrOne : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(N->getZExtValue() >> 1, MVT::i32);
}]>;
def shrTwo : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(N->getZExtValue() >> 2, MVT::i32);
}]>;


//===----------------------------------------------------------------------===//
// Patmos Complex Pattern Definitions.
//===----------------------------------------------------------------------===//

/*
def addr : ComplexPattern<iPTR, 2, "SelectAddr", [], []>;
*/

//===----------------------------------------------------------------------===//
// Pattern Fragments
//===----------------------------------------------------------------------===//

// offset of indexed load/store
def imm7     : PatLeaf<(imm), [{return isInt <7>(N->getSExtValue());}]>;
def uimm7    : PatLeaf<(imm), [{return isUInt<7>(N->getZExtValue());}]>;
// ALUi
def uimm12   : PatLeaf<(imm), [{return isUInt<12>(N->getZExtValue());}]>;
// Call/Branch/StackCtrl
def uimm22   : PatLeaf<(imm), [{return isUInt<22>(N->getZExtValue());}]>;


// 7 bit value, shifted by one bit (for 16-bit stores)
def uimm7s1 : Operand<i32>, PatLeaf<(imm), [{
 return isUInt<7>(N->getZExtValue() >> 1) && ((N->getZExtValue() & 0x01) == 0);
}], shrOne>;
// 7 bit value, shifted by two bits (for 32-bit stores)
def uimm7s2 : Operand<i32>, PatLeaf<(imm), [{
 return isUInt<7>(N->getZExtValue() >> 2) && ((N->getZExtValue() & 0x03) == 0);
}], shrTwo>;



// Helper fragment for predicate inversion
def notcc  : PatFrag<(ops node:$in), (i1 (setne node:$in, (i1 -1)))>;


//===----------------------------------------------------------------------===//
// Instruction list..
//===----------------------------------------------------------------------===//


// ADJCALLSTACKDOWN/UP implicitly use/def SP because they may be expanded into
// a stack adjustment and the codegen must know that they may modify the stack
// pointer before prolog-epilog rewriting occurs.
// Pessimistically assume ADJCALLSTACKDOWN / ADJCALLSTACKUP will become
// sub / add which can clobber SP.
let Defs = [R30, R31], Uses = [R30, R31] in {
def ADJCALLSTACKDOWN : PatmosInst<(outs), (ins i32imm:$amt),
                                    "#ADJCALLSTACKDOWN",
                                    [(callseq_start timm:$amt)]>;
def ADJCALLSTACKUP   : PatmosInst<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                                    "#ADJCALLSTACKUP",
                                    [(callseq_end timm:$amt1, timm:$amt2)]>;
}



//===----------------------------------------------------------------------===//
//  Control Flow Instructions...
//===----------------------------------------------------------------------===//

let isReturn = 1, isTerminator = 1, isBarrier = 1 in {
  def RET  : PFLr<0b0000, (outs), (ins), "ret # FIXME delay slot",  [(retflag)]>;
}


//===----------------------------------------------------------------------===//
//  Call Instructions...
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
//  Miscellaneous Instructions...
//===----------------------------------------------------------------------===//



//===----------------------------------------------------------------------===//
// Move Instructions
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
// Arithmetic Instructions
//===----------------------------------------------------------------------===//

multiclass ALU_ilr <string asmstr, bits<4> func, SDNode opnode> {
  def i : ALUi<func{1-3}, (outs RRegs:$rd), (ins RRegs:$rs1, i32imm:$imm),
                !strconcat(asmstr, " $rd, $rs1, $imm"),
                [(set RRegs:$rd, (opnode RRegs:$rs1, (i32 uimm12:$imm)))]>;

  def l : ALUl<func, (outs RRegs:$rd), (ins RRegs:$rs1, i32imm:$imm),
                !strconcat(asmstr, " $rd, $rs1, $imm"),
                [(set RRegs:$rd, (opnode RRegs:$rs1, (i32 imm:$imm)))]>;

  def r : ALUr<func, (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
                !strconcat(asmstr, " $rd, $rs1, $rs2"),
                [(set RRegs:$rd, (opnode RRegs:$rs1, RRegs:$rs2))]>;
}


multiclass ALU_lr <string asmstr, bits<4> func, SDNode opnode> {
  def l : ALUl<func, (outs RRegs:$rd), (ins RRegs:$rs1, i32imm:$imm),
                !strconcat(asmstr, " $rd, $rs1, $imm"),
                [(set RRegs:$rd, (opnode RRegs:$rs1, (i32 imm:$imm)))]>;

  def r : ALUr<func, (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
                !strconcat(asmstr, " $rd, $rs1, $rs2"),
                [(set RRegs:$rd, (opnode RRegs:$rs1, RRegs:$rs2))]>;
}


/** also short immediates ******/

let isCommutable = 1 in {
defm ADD : ALU_ilr <"add", 0b0000, add>;

defm OR  : ALU_ilr <"or" , 0b0110, or >;
defm AND : ALU_ilr <"and", 0b0111, and>;
}

defm SUB  : ALU_ilr <"sub",  0b0001, sub>;
defm SL   : ALU_ilr <"sl",   0b0011, shl>;
defm SR   : ALU_ilr <"sr",   0b0100, srl>;
defm SRA  : ALU_ilr <"sra",  0b0101, sra>;



//TODO beautify with ALU_ilr multiclass
def RSUBi : ALUi<0b0010, (outs RRegs:$rd), (ins RRegs:$rs1, i32imm:$imm),
                "rsub $rd, $rs1, $imm",
                [(set RRegs:$rd, (sub (i32 uimm12:$imm), RRegs:$rs1))]>;
def RSUBl : ALUl<0b0010, (outs RRegs:$rd), (ins RRegs:$rs1, i32imm:$imm),
                "rsub $rd, $rs1, $imm",
                [(set RRegs:$rd, (sub (i32 imm:$imm), RRegs:$rs1))]>;
def RSUBr : ALUr<0b0010, (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
                "rsub $rd, $rs1, $rs2",
                [(set RRegs:$rd, (sub RRegs:$rs2, RRegs:$rs1))]>;



/********/

defm RL   : ALU_lr <"rl",   0b1000, rotl>;
defm RR   : ALU_lr <"rr",   0b1001, rotr>;

let isCommutable = 1 in {
defm XOR  : ALU_lr <"xor" , 0b1010, or >;

def NORl  : ALUl <0b1011,
              (outs RRegs:$rd), (ins RRegs:$rs1, i32imm:$imm),
              "nor $rd, $rs1, $imm",
              [(set RRegs:$rd, (not (or RRegs:$rs1, imm:$imm)))]>;
def NORr  : ALUl <0b1011,
              (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
              "nor $rd, $rs1, $rs2",
              [(set RRegs:$rd, (not (or RRegs:$rs1, RRegs:$rs2)))]>;
}

//TODO beautify with ALU_ilr multiclass
def SHADDl  : ALUl<0b1100, (outs RRegs:$rd), (ins RRegs:$rs1, i32imm:$imm),
                  "shadd $rd, $rs1, $imm",
                [(set RRegs:$rd, (add (shl RRegs:$rs1, (i32 1)), (i32 imm:$imm)))]>;
def SHADDr  : ALUr<0b1100, (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
                  "shadd $rd, $rs1, $rs2",
                [(set RRegs:$rd, (add (shl RRegs:$rs1, (i32 1)), RRegs:$rs2))]>;
def SHADD2l : ALUl<0b1101, (outs RRegs:$rd), (ins RRegs:$rs1, i32imm:$imm),
                  "shadd2 $rd, $rs1, $imm",
                [(set RRegs:$rd, (add (shl RRegs:$rs1, (i32 2)), (i32 imm:$imm)))]>;
def SHADD2r : ALUr<0b1101, (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
                  "shadd2 $rd, $rs1, $rs2",
                [(set RRegs:$rd, (add (shl RRegs:$rs1, (i32 2)), RRegs:$rs2))]>;


// Pseudos


// Pseudo instr MOV uses add opcode
let isAsCheapAsAMove = 1 in
def MOV : ALUi<0b0000, (outs RRegs:$rd), (ins RRegs:$rs),
                "mov $rd, $rs", []>;

// Pseudo instr NEG uses rsub opcode
let Imm12 = 0 in
def NEG : ALUi<0b0010, (outs RRegs:$rd), (ins RRegs:$rs),
                "neg $rd, $rs",
                [(set RRegs:$rd, (ineg RRegs:$rs))]>;

// Pseudo instr NOT uses rsub opcode (maybe prefer xor?)
def NOT : ALUl<0b1011, (outs RRegs:$rd), (ins RRegs:$rs),
                "not $rd, $rs",
                [(set RRegs:$rd, (not RRegs:$rs))]>;

// Pseudo instr ZEXT8 uses 'and' opcode
def ZEXT8 : ALUi <0b0111,
              (outs RRegs:$rd), (ins RRegs:$rs),
              "zext8 $rd, $rs",
              [(set RRegs:$rd, (and RRegs:$rs, (i32 0xff)))]>;

// Pseudo instr LI uses add opcode
let Rs1 = 0,
    isReMaterializable = 1,
    isAsCheapAsAMove = 1,
    isMoveImm = 1 in {
def LIi : ALUl<0b0000, (outs RRegs:$rd), (ins i32imm:$imm),
                "li $rd, $imm",
                [(set RRegs:$rd, (add R0, (i32 uimm12:$imm)))]>;
def LIl : ALUl<0b0000, (outs RRegs:$rd), (ins i32imm:$imm),
                "li $rd, $imm",
                [(set RRegs:$rd, (add R0, (i32 imm:$imm)))]>;
}

/********/
// Unary Arithmetic

def SEXT8  : ALUu <0b0000,
              (outs RRegs:$rd), (ins RRegs:$rs),
              "sext8 $rd, $rs",
              [(set RRegs:$rd, (sext_inreg RRegs:$rs, i8))]>;

def SEXT16 : ALUu <0b0001,
              (outs RRegs:$rd), (ins RRegs:$rs),
               "sext16 $rd, $rs",
              [(set RRegs:$rd, (sext_inreg RRegs:$rs, i16))]>;

def ZEXT16 : ALUu <0b0010,
              (outs RRegs:$rd), (ins RRegs:$rs),
              "zext16 $rd, $rs",
              [(set RRegs:$rd, (and RRegs:$rs, (i32 0xffff)))]>;

/*
def CLZ    : ALUu <0b0011,
              (outs RRegs:$rd), (ins RRegs:$rs1),
              "clz $rd, $rs1",
              [(set RRegs:$rd, (ctlz RRegs:$rs1))]>;

def CTZ    : ALUu <0b0100,
              (outs RRegs:$rd), (ins RRegs:$rs1),
              "ctz $rd, $rs1",
              [(set RRegs:$rd, (cttz RRegs:$rs1))]>;
*/

// TODO experimental: other lowerings?
// TODO does the add overflow do any harm?
def ABS    : ALUu <0b0011,
              (outs RRegs:$rd), (ins RRegs:$rs),
              "abs $rd, $rs",
              [(set RRegs:$rd, (xor (add RRegs:$rs, (sra RRegs:$rs, (i32 31))), (sra RRegs:$rs, (i32 31))))]>;





// Multiply

// TODO
/*
let Defs = [SL,SH], isCommutable=1 in {
def MUL  : ALUm <0b0000,
              (outs Mul:$sd), (ins RRegs:$rs1, RRegs:$rs2),
              "mul $$$sd, $rs1, $rs2",
              [(set Mul:$sd, (patmos_mul RRegs:$rs1, RRegs:$rs2))]>;

def MULU : ALUm <0b0001,
              (outs), (ins RRegs:$rs1, RRegs:$rs2),
              "mulu $rs1, $rs2",
              []>;
              //[(set SRegs:$sl, (mul RRegs:$rs1, RRegs:$rs2))]>;
}
*/

// Compare

multiclass ALUc_cn <string asmstr, bits<4> func, PatFrag cnode, PatFrag nnode> {
  def c : ALUc<func, (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
                !strconcat(asmstr, " $rd, $rs1, $rs2"),
                [(set RRegs:$rd, (cnode RRegs:$rs1, RRegs:$rs2))]>;

  def n : ALUc<func, (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
                !strconcat(asmstr, " $rd, $rs1, $rs2"),
                [(set RRegs:$rd, (nnode RRegs:$rs1, RRegs:$rs2))]>;
}

defm CMPEQ  : ALUc_cn <"cmpeq",  0b0000, seteq,  seteq>;
defm CMPNEQ : ALUc_cn <"cmpneq", 0b0001, setne,  setne>;
defm CMPLT  : ALUc_cn <"cmplt",  0b0010, setlt,  setgt>;
defm CMPLE  : ALUc_cn <"cmple",  0b0011, setle,  setge>;
defm CMPULT : ALUc_cn <"cmpult", 0b0100, setult, setugt>;
defm CMPULE : ALUc_cn <"cmpule", 0b0101, setule, setuge>;

def BTEST : ALUc<0b0110, (outs RRegs:$rd), (ins RRegs:$rs1, RRegs:$rs2),
                  "btest $rd, $rs1, $rs2",
                  [(set RRegs:$rd, (setne (and RRegs:$rs1, (shl (i32 1), RRegs:$rs2)), (i32 0)))]>;

// isodd Pd = Rs1 ... btest Pd = Rs1[r0]



// Predicate

//or
def POR: ALUp <0b0110, (outs PRegs:$pd), (ins PRegs:$ps1, PRegs:$ps2),
                "or $pd, $ps1, $ps2",
                [(set PRegs:$pd, (or PRegs:$ps1, PRegs:$ps2))]>;

//and
def PAND: ALUp <0b0110, (outs PRegs:$pd), (ins PRegs:$ps1, PRegs:$ps2),
                "and $pd, $ps1, $ps2",
                [(set PRegs:$pd, (and PRegs:$ps1, PRegs:$ps2))]>;
//xor
def PXOR: ALUp <0b1010, (outs PRegs:$pd), (ins PRegs:$ps1, PRegs:$ps2),
                "xor $pd, $ps1, $ps2",
                [(set PRegs:$pd, (xor PRegs:$ps1, PRegs:$ps2))]>;
//nor
def PNOR: ALUp <0b1010, (outs PRegs:$pd), (ins PRegs:$ps1, PRegs:$ps2),
                "xor $pd, $ps1, $ps2",
                [(set PRegs:$pd, (notcc (or PRegs:$ps1, PRegs:$ps2)))]>;


// Pseudos:
//mov
//not
//set
//clr


// NOP
let neverHasSideEffects = 1 in
def NOP  : SPCn <(outs), (ins i32imm:$imm), "nop $imm", []>;

// Wait
def WAIT : SPCw <0b0000, (outs), (ins), "wait.mem", []>;

// move to special
def MTS  : SPCt <(outs SRegs:$sd), (ins RRegs:$rs), "mts $sd, $rs", [] >;

// move from special
def MFS  : SPCf <(outs RRegs:$rd), (ins SRegs:$ss), "mfs $rd, $ss", [] >;



// Load typed

//FIXME
def LWM : LDT <0b00011, (outs RRegs:$rd), (ins RRegs:$ra), "lwm $rd, $$ra", [(set RRegs:$rd, (load RRegs:$ra))]>;


// Store typed

//def SWM   : STT <0b00011, (outs ), (ins RRegs:$ra, RRegs:$rs), "swm $$ra, $rs", [(store RRegs:$ra, RRegs:$rs)]>;

def SWMp  : STT<0b00011, (outs ), (ins RRegs:$ra, RRegs:$rs),
                "swm 0[$ra], $rs", [(store RRegs:$ra, RRegs:$rs)]>;
def SWMpi : STT<0b00011, (outs ), (ins RRegs:$ra, RRegs:$rs, i32imm:$off),
                "swm $off[$ra], $rs",  [(store (add RRegs:$ra, (i32 uimm7s2:$off)), RRegs:$rs)]>;




//===----------------------------------------------------------------------===//
// Non-Instruction Patterns
//===----------------------------------------------------------------------===//


// load immediate

def : Pat<(i32 imm:$imm),
          (LIi (i32 uimm12:$imm) )>;

def : Pat<(i32 imm:$imm),
          (LIl (i32 imm:$imm) )>;

// multiplication
// TODO
//def : Pat<(mul RRegs:$rs1, RRegs:$rs2),
//          (COPY_TO_REGCLASS (MUL RRegs:$rs1, RRegs:$rs2), RRegs)>;

