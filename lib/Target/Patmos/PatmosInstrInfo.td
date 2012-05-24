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

def SDT_PatmosCall         : SDTypeProfile<0, -1, [SDTCisVT<0, iPTR>]>;
def SDT_PatmosCallSeqStart : SDCallSeqStart<[SDTCisI32<0>]>;
def SDT_PatmosCallSeqEnd   : SDCallSeqEnd<[SDTCisI32<0>, SDTCisI32<1>]>;

//===----------------------------------------------------------------------===//
// Patmos Specific Node Definitions.
//===----------------------------------------------------------------------===//

def retflag       : SDNode<"PatmosISD::RET_FLAG", SDTNone,
                          [SDNPHasChain, SDNPOptInGlue]>;

def PatmosCall    : SDNode<"PatmosISD::CALL",SDT_PatmosCall,
                         [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                          SDNPVariadic]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_PatmosCallSeqStart,
                          [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_PatmosCallSeqEnd,
                          [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def patmos_mul    : SDNode<"PatmosISD::MUL", SDTIntBinOp,
                          [SDNPCommutative, SDNPAssociative]>;


//===----------------------------------------------------------------------===//
// Patmos Operand Definitions.
//===----------------------------------------------------------------------===//

// Patmos predicate operand.
// actually this is a pair of MachineOperands, where
// the first is the predicate register
// and the second is the inversion flag
def pred : PredicateOperand<OtherVT, (ops PRegs, i1imm),
                                     (ops (i32 zero_reg), (i1 0))> {
  let PrintMethod = "printPredicateOperand";
}

// for cache relative branches
def brtarget : Operand<OtherVT> {
  //let PrintMethod = "printCacheRelImmOperand";
}

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

// NOP
def uimm4    : PatLeaf<(imm), [{return isUInt<4>(N->getZExtValue());}]>;

// offset of indexed load/store
def imm7     : Operand<i32>, PatLeaf<(imm), [{return isInt <7>(N->getSExtValue());}]>;
def uimm7    : PatLeaf<(imm), [{return isUInt<7>(N->getZExtValue());}]>;

// ALUi
def uimm12   : PatLeaf<(imm), [{return isUInt<12>(N->getZExtValue());}]>;
// Call/Branch/StackCtrl
def uimm22   : Operand<i32>, PatLeaf<(imm), [{return isUInt<22>(N->getZExtValue());}]>;


// 7 bit value, shifted by one bit (for 16-bit stores)
def imm7s1 : Operand<i32>, PatLeaf<(imm), [{
 return isInt<7>(N->getSExtValue() >> 1) && ((N->getSExtValue() & 0x01) == 0);
}], shrOne>;
def uimm7s1 : Operand<i32>, PatLeaf<(imm), [{
 return isUInt<7>(N->getZExtValue() >> 1) && ((N->getZExtValue() & 0x01) == 0);
}], shrOne>;
// 7 bit value, shifted by two bits (for 32-bit stores)
def imm7s2 : Operand<i32>, PatLeaf<(imm), [{
 return isInt<7>(N->getSExtValue() >> 2) && ((N->getSExtValue() & 0x03) == 0);
}], shrTwo>;
def uimm7s2 : Operand<i32>, PatLeaf<(imm), [{
 return isUInt<7>(N->getZExtValue() >> 2) && ((N->getZExtValue() & 0x03) == 0);
}], shrTwo>;

// Transform all FrameIndex objects into TargetFrameIndex objects and define a
// pattern that allows us to match against FrameIndex patterns
def FI_XForm : SDNodeXForm<frameindex, [{ return CurDAG->getTargetFrameIndex(N->getIndex(), N->getValueType(0)); }]>;
def fipat : PatLeaf<(i32 frameindex), [{}], FI_XForm>;

def GS_XForm : SDNodeXForm<globaladdr, [{ return CurDAG->getTargetGlobalAddress(N->getGlobal(), N->getDebugLoc(), N->getValueType(0), N->getOffset(), N->getTargetFlags()); }]>;
def gspat : PatLeaf<(i32 globaladdr), [{}], GS_XForm>;

def ES_XForm : SDNodeXForm<externalsym, [{ return CurDAG->getTargetExternalSymbol(N->getSymbol(), N->getValueType(0), N->getTargetFlags()); }]>;
def espat : PatLeaf<(i32 externalsym), [{}], ES_XForm>;

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
let Defs = [RFP, RSP], Uses = [RFP, RSP] in {
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

let isReturn=1, isTerminator=1, isBarrier=1, hasDelaySlot=1,
    Uses = [SB, SO] in {
  def RET  : PFLr<0b0000, (outs), (ins pred:$p),
                  "$p ret # FIXME delay slot",  [(retflag)]>;
}

let isTerminator=1, isBarrier=1, hasDelaySlot=1 in {
  let isBranch=1 in
  def BC  : PFLb<0b01  , (outs), (ins pred:$p, brtarget:$target),
                  "$p bc $target # FIXME delay slot",  []>;

  let isIndirectBranch=1 in
  def BCR : PFLi<0b0001, (outs), (ins pred:$p, RRegs:$rs),
                  "$p bcr $rs # FIXME delay slot",  []>;

}

def : Pat<(br bb:$target),   (BC  bb:$target)>;
def : Pat<(brind RRegs:$rs), (BCR RRegs:$rs)>;



//===----------------------------------------------------------------------===//
//  Call Instructions...
//===----------------------------------------------------------------------===//

let isCall=1, hasDelaySlot=1,
  // Calls may overwrite caller saved registers
  Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16,
          R17, R18, R19, R20,
          SB, SO, SM, SL, SH, S7, S8, S9, S10, S11, S12, S13, S14, S15] in {

  def bs : PFLb<0b00  , (outs), (ins pred:$p, uimm22:$imm),
                    "$p bs $imm # FIXME delay slot",
                    [(PatmosCall uimm22:$imm)]>;

  def bsr: PFLi<0b0000, (outs), (ins pred:$p, RRegs:$rs1),
                    "$p bsr $rs1 # FIXME delay slot",
                    [(PatmosCall RRegs:$rs1)]>;
}

//===----------------------------------------------------------------------===//
//  Miscellaneous Instructions...
//===----------------------------------------------------------------------===//



//===----------------------------------------------------------------------===//
// Move Instructions
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
// Arithmetic Instructions
//===----------------------------------------------------------------------===//

// no short immediates
multiclass BinArith<bits<4> func, string asmstr, SDNode opnode, bit isrCommutable=0> {
  def l : ALUl<func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
                !strconcat( "$p ", !strconcat(asmstr, " $rd, $rs1, $imm")),
                [(set RRegs:$rd, (opnode RRegs:$rs1, (i32 imm:$imm)))]>;

  let isCommutable = isrCommutable in
  def r : ALUr<func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
                !strconcat( "$p ", !strconcat(asmstr, " $rd, $rs1, $rs2")),
                [(set RRegs:$rd, (opnode RRegs:$rs1, RRegs:$rs2))]>;
}

// also support short immediates
multiclass BinArithI<bits<4> func, string asmstr, SDNode opnode, bit isrCommutable=0> {
  def i : ALUi<func{1-3}, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
                !strconcat( "$p ", !strconcat(asmstr, " $rd, $rs1, $imm")),
                [(set RRegs:$rd, (opnode RRegs:$rs1, (i32 uimm12:$imm)))]>;
  // also L and R
  defm NAME : BinArith<func, asmstr, opnode, isrCommutable>;
}



/** also short immediates ******/

defm ADD : BinArithI<0b0000, "add", add, 1>;

defm OR  : BinArithI<0b0110, "or ", or , 1>;
defm AND : BinArithI<0b0111, "and", and, 1>;

defm SUB : BinArithI<0b0001, "sub", sub>;
defm SL  : BinArithI<0b0011, "sl ", shl>;
defm SR  : BinArithI<0b0100, "sr ", srl>;
defm SRA : BinArithI<0b0101, "sra", sra>;



//TODO beautify with ALU_ilr multiclass
def RSUBi : ALUi<0b0010, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
                "$p rsub $rd, $rs1, $imm",
                [(set RRegs:$rd, (sub (i32 uimm12:$imm), RRegs:$rs1))]>;
def RSUBl : ALUl<0b0010, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
                "$p rsub $rd, $rs1, $imm",
                [(set RRegs:$rd, (sub (i32 imm:$imm), RRegs:$rs1))]>;
def RSUBr : ALUr<0b0010, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
                "$p rsub $rd, $rs1, $rs2",
                [(set RRegs:$rd, (sub RRegs:$rs2, RRegs:$rs1))]>;



/********/

defm RL   : BinArith <0b1000, "rl ", rotl>;
defm RR   : BinArith <0b1001, "rr ", rotr>;

defm XOR  : BinArith<0b1010, "xor", xor , 1>;

def NORl  : ALUl <0b1011,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
              "$p, nor $rd, $rs1, $imm",
              [(set RRegs:$rd, (not (or RRegs:$rs1, imm:$imm)))]>;

let isCommutable = 1 in
def NORr  : ALUr <0b1011,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
              "$p nor $rd, $rs1, $rs2",
              [(set RRegs:$rd, (not (or RRegs:$rs1, RRegs:$rs2)))]>;

//TODO beautify with  multiclass
def SHADDl  : ALUl<0b1100, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
                  "$p shadd $rd, $rs1, $imm",
                [(set RRegs:$rd, (add (shl RRegs:$rs1, (i32 1)), (i32 imm:$imm)))]>;
def SHADDr  : ALUr<0b1100, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
                  "$p shadd $rd, $rs1, $rs2",
                [(set RRegs:$rd, (add (shl RRegs:$rs1, (i32 1)), RRegs:$rs2))]>;
def SHADD2l : ALUl<0b1101, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
                  "$p shadd2 $rd, $rs1, $imm",
                [(set RRegs:$rd, (add (shl RRegs:$rs1, (i32 2)), (i32 imm:$imm)))]>;
def SHADD2r : ALUr<0b1101, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
                  "$p shadd2 $rd, $rs1, $rs2",
                [(set RRegs:$rd, (add (shl RRegs:$rs1, (i32 2)), RRegs:$rs2))]>;


// Pseudos


// Pseudo instr MOV uses add opcode
let isAsCheapAsAMove = 1 in
def MOV : ALUi<0b0000, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
                "$p mov $rd, $rs", []>;

// Pseudo instr NEG uses rsub opcode
let Imm12 = 0 in
def NEG : ALUi<0b0010, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
                "$p neg $rd, $rs",
                [(set RRegs:$rd, (ineg RRegs:$rs))]>;

// Pseudo instr NOT uses rsub opcode (maybe prefer xor?)
def NOT : ALUr<0b1011, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
                "$p not $rd, $rs",
                [(set RRegs:$rd, (not RRegs:$rs))]>;

// Pseudo instr ZEXT8 uses 'and' opcode
def ZEXT8 : ALUi <0b0111,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
              "$p zext8 $rd, $rs",
              [(set RRegs:$rd, (and RRegs:$rs, (i32 0xff)))]>;

// Pseudo instr LI uses add opcode
let Rs1 = 0,
    isReMaterializable = 1,
    isAsCheapAsAMove = 1,
    isMoveImm = 1 in {
def LIi : ALUl<0b0000, (outs RRegs:$rd), (ins pred:$p, i32imm:$imm),
                "$p li $rd, $imm",
                [(set RRegs:$rd, (add R0, (i32 uimm12:$imm)))]>;
def LIl : ALUl<0b0000, (outs RRegs:$rd), (ins pred:$p, i32imm:$imm),
                "$p li $rd, $imm",
                [(set RRegs:$rd, (add R0, (i32 imm:$imm)))]>;
}

/********/
// Unary Arithmetic

def SEXT8  : ALUu <0b0000,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
              "$p sext8 $rd, $rs",
              [(set RRegs:$rd, (sext_inreg RRegs:$rs, i8))]>;

def SEXT16 : ALUu <0b0001,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
               "$p sext16 $rd, $rs",
              [(set RRegs:$rd, (sext_inreg RRegs:$rs, i16))]>;

def ZEXT16 : ALUu <0b0010,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
              "$p zext16 $rd, $rs",
              [(set RRegs:$rd, (and RRegs:$rs, (i32 0xffff)))]>;


// TODO experimental: other lowerings?
// TODO does the add overflow do any harm?
def ABS    : ALUu <0b0011,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
              "$p abs $rd, $rs",
              [(set RRegs:$rd, (xor (add RRegs:$rs, (sra RRegs:$rs, (i32 31))), (sra RRegs:$rs, (i32 31))))]>;





// Multiply

// TODO
/*
let Defs = [SL,SH], isCommutable=1 in {
def MUL  : ALUm <0b0000,
              (outs Mul:$sd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
              "$p mul $$$sd, $rs1, $rs2",
              [(set Mul:$sd, (patmos_mul RRegs:$rs1, RRegs:$rs2))]>;

def MULU : ALUm <0b0001,
              (outs), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
              "$p mulu $rs1, $rs2",
              []>;
              //[(set SRegs:$sl, (mul RRegs:$rs1, RRegs:$rs2))]>;
}
*/

// Compare

multiclass ALUc_cn <string asmstr, bits<4> func, PatFrag cnode, PatFrag nnode> {
  def c : ALUc<func, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
                !strconcat( "$p ", !strconcat(asmstr, " $pd, $rs1, $rs2")),
                [(set PRegs:$pd, (cnode RRegs:$rs1, RRegs:$rs2))]>;

  def n : ALUc<func, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
                !strconcat( "$p ", !strconcat(asmstr, " $pd, $rs1, $rs2")),
                [(set PRegs:$pd, (nnode RRegs:$rs1, RRegs:$rs2))]>;
}

// we simply invert setcc condition instead of emitting pseudo instructions
defm CMPEQ  : ALUc_cn <"cmpeq",  0b0000, seteq,  seteq>;
defm CMPNEQ : ALUc_cn <"cmpneq", 0b0001, setne,  setne>;
defm CMPLT  : ALUc_cn <"cmplt",  0b0010, setlt,  setgt>;
defm CMPLE  : ALUc_cn <"cmple",  0b0011, setle,  setge>;
defm CMPULT : ALUc_cn <"cmpult", 0b0100, setult, setugt>;
defm CMPULE : ALUc_cn <"cmpule", 0b0101, setule, setuge>;

def  BTEST  : ALUc<0b0110, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
                  "$p btest $pd, $rs1, $rs2",
                  [(set PRegs:$pd, (setne (and RRegs:$rs1, (shl (i32 1), RRegs:$rs2)), (i32 0)))]>;

// Pseudo instr ISODD uses btest opcode
let Rs2 = 0 in
def  ISODD  : ALUc<0b0110, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs),
                  "$p isodd $pd, $rs",
                  [(set PRegs:$pd, (setne (and RRegs:$rs, (i32 1)), (i32 1)))]>;

// Pseudo mov Pd <- Rs uses cmpneq with r0
// I don't think it makes sense to define it at all
let Rs2 = 0 in
def  MOVrp  : ALUc<0b0001, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs),
                  "$p mov $pd, $rs",
                  [(set PRegs:$pd, (setne RRegs:$rs, (i32 0)))]>;



// Predicate

//or
def POR: ALUp <0b0110, (outs PRegs:$pd), (ins pred:$p, PRegs:$ps1, PRegs:$ps2),
                "$p or $pd, $ps1, $ps2",
                [(set PRegs:$pd, (or PRegs:$ps1, PRegs:$ps2))]>;

//and
def PAND: ALUp <0b0110, (outs PRegs:$pd), (ins pred:$p, PRegs:$ps1, PRegs:$ps2),
                "$p and $pd, $ps1, $ps2",
                [(set PRegs:$pd, (and PRegs:$ps1, PRegs:$ps2))]>;
//xor
def PXOR: ALUp <0b1010, (outs PRegs:$pd), (ins pred:$p, PRegs:$ps1, PRegs:$ps2),
                "$p xor $pd, $ps1, $ps2",
                [(set PRegs:$pd, (xor PRegs:$ps1, PRegs:$ps2))]>;
//nor
def PNOR: ALUp <0b1010, (outs PRegs:$pd), (ins pred:$p, PRegs:$ps1, PRegs:$ps2),
                "$p nor $pd, $ps1, $ps2",
                [(set PRegs:$pd, (notcc (or PRegs:$ps1, PRegs:$ps2)))]>;


// Pseudos:
//mov uses or opcode (or Pd = Ps | Ps)
//let Ps2 = Ps1 in
def PMOV: ALUp <0b0110, (outs PRegs:$pd), (ins pred:$p, PRegs:$ps),
                "$p mov $pd, $ps", []>;

//not uses nor opcode
//let Ps2 = Ps1 in
def PNOT: ALUp <0b1011, (outs PRegs:$pd), (ins pred:$p, PRegs:$ps),
                "$p not $pd, $ps",
                [(set PRegs:$pd, (not PRegs:$ps))]>;

//set uses or opcode
let Ps1 = 0, Ps2 = 0 in
def PSET: ALUp <0b0110, (outs PRegs:$pd), (ins pred:$p),
                "$p set $pd", [(set PRegs:$pd, (i1 1))]>;
//clr uses nor opcode
let Ps1 = 0, Ps2 = 0 in
def PCLR: ALUp <0b1010, (outs PRegs:$pd), (ins pred:$p),
                "$p clr $pd", [(set PRegs:$pd, (i1 0))]>;


// NOP
let neverHasSideEffects = 1 in
def NOP  : SPCn <(outs), (ins pred:$p, i32imm:$imm), "$p nop $imm", []>;

// Wait
def WAIT : SPCw <0b0000, (outs), (ins pred:$p), "$p wait.mem", []>;

// move to special
def MTS  : SPCt <(outs SRegs:$sd), (ins pred:$p, RRegs:$rs), "$p mts $sd, $rs", [] >;

// move from special
def MFS  : SPCf <(outs RRegs:$rd), (ins pred:$p, SRegs:$ss), "$p mfs $rd, $ss", [] >;



// Load typed

multiclass LoadTyped<bits<3> ty, Operand immTy, string asmop>
{
  // load from stack cache
  def S  : LDT<ty, 0b0, 0b0, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               !strconcat(!strconcat("$p $rd = ", asmop), "s [$ra + $imm];"),
               [] >;

  // load from local memory
  def L  : LDT<ty, 0b0, 0b1, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               !strconcat(!strconcat("$p $rd = ", asmop), "l [$ra + $imm];"),
               [] >;

  // load from data cache
  def C  : LDT<ty, 0b1, 0b0, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               !strconcat(!strconcat("$p $rd = ", asmop), "c [$ra + $imm];"),
               [] >;

  // load from main memory
  def M  : LDT<ty, 0b1, 0b1, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               !strconcat(!strconcat("$p $rd = ", asmop), "m [$ra + $imm];"),
               [] >;
}

multiclass DecoupledLoadTyped<bits<4> ty, Operand immTy, string asmop>
{
  let Defs = [SM] in
  {
    // load from data cache (no pattern to match)
    def C  : LDT<ty{3-1}, ty{0}, 0b0, (outs /*empty*/),
                 (ins pred:$p, RRegs:$ra, immTy:$imm),
                 !strconcat(!strconcat("$p sm = d", asmop), "c [$ra + $imm];"),
                 [] >;

    // load from main memory (no pattern to match)
    def M  : LDT<ty{3-1}, ty{0}, 0b1, (outs /*empty*/),
                 (ins pred:$p, RRegs:$ra, immTy:$imm),
                 !strconcat(!strconcat("$p sm = d", asmop), "m [$ra + $imm];"),
                 [] >;
  }
}

multiclass LoadTypedPatterns<PatmosInst inst_C, PatFrag immFg, PatFrag pfg>
{
  // load register indirect from cache
  def ra : Pat<(pfg RRegs:$ra), (inst_C RRegs:$ra, 0)>;

  // load register indirect + immediate from cache
  def raimm : Pat<(pfg (add RRegs:$ra, immFg:$imm)), 
                  (inst_C RRegs:$ra, immFg:$imm)>;

  // load register indirect from stack cache
  def fi : Pat<(pfg fipat:$fi), (inst_C fipat:$fi, 0)>;

  // load register indirect from stack cache
  def fiimm : Pat<(pfg (add fipat:$fi, immFg:$imm)),
                  (inst_C fipat:$fi, immFg:$imm)>;

  // TODO: more patterns here
}

defm LW  : LoadTyped<0b000, imm7s2, "lw" >;
defm LH  : LoadTyped<0b001, imm7s1, "lh" >;
defm LB  : LoadTyped<0b010, imm7  , "lb" >;
defm LHU : LoadTyped<0b011, imm7s1, "lhu">;
defm LBU : LoadTyped<0b100, imm7  , "lbu">;

defm DLW  : DecoupledLoadTyped<0b1010, imm7s2, "lw" >;
defm DLH  : DecoupledLoadTyped<0b1011, imm7s1, "lh" >;
defm DLB  : DecoupledLoadTyped<0b1100, imm7  , "lb" >;
defm DLHU : DecoupledLoadTyped<0b1101, imm7s1, "lhu">;
defm DLBU : DecoupledLoadTyped<0b1110, imm7  , "lbu">;

defm LW  : LoadTypedPatterns<LWC , imm7s2, load>;
defm LH  : LoadTypedPatterns<LHC , imm7s1, sextloadi16>;
defm LB  : LoadTypedPatterns<LBC , imm7  , sextloadi8>;
defm LHU : LoadTypedPatterns<LHUC, imm7s1, zextloadi16>;
defm LBU : LoadTypedPatterns<LBUC, imm7  , zextloadi8>;


// Store typed

multiclass StoreTyped<bits<3> ty, Operand immTy, string asmop>
{
  // store to stack cache
  def S  : STT<ty, 0b00, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               !strconcat("$p ", !strconcat(asmop, "s [$ra + $imm] = $rs;")),
               [] >;

  // store to local memory
  def L  : STT<ty, 0b01, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               !strconcat("$p ", !strconcat(asmop, "l [$ra + $imm] = $rs;")),
               [] >;

  // store to data cache
  def C  : STT<ty, 0b10, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               !strconcat("$p ", !strconcat(asmop, "c [$ra + $imm] = $rs;")),
               [] >;

  // store to stack cache
  def M  : STT<ty, 0b11, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               !strconcat("$p ", !strconcat(asmop, "m [$ra + $imm] = $rs;")),
               [] >;
}

multiclass StoreTypedPatterns<PatmosInst inst_C, PatFrag immFg, PatFrag pfg>
{
  // load register indirect from cache
  def ra : Pat<(pfg RRegs:$rs, RRegs:$ra),
               (inst_C RRegs:$ra, 0, RRegs:$rs)>;

  // load register indirect + immediate from cache
  def raimm : Pat<(pfg RRegs:$rs, (add RRegs:$ra, immFg:$imm)),
                  (inst_C RRegs:$ra, immFg:$imm, RRegs:$rs)>;

  // load register indirect from stack cache
  def fi : Pat<(pfg RRegs:$rs, fipat:$fi),
               (inst_C fipat:$fi, 0, RRegs:$rs)>;

  // load register indirect from stack cache
  def fiimm : Pat<(pfg RRegs:$rs, (add fipat:$fi, immFg:$imm)),
                  (inst_C fipat:$fi, immFg:$imm, RRegs:$rs)>;

  // TODO: more patterns here
}

defm SW  : StoreTyped<0b000, imm7s2, "sw">;
defm SH  : StoreTyped<0b001, imm7s1, "sh">;
defm SB  : StoreTyped<0b010, imm7  , "sb">;

defm SW  : StoreTypedPatterns<SWC, imm7s2, store>;
defm SH  : StoreTypedPatterns<SHC, imm7s1, truncstorei16>;
defm SB  : StoreTypedPatterns<SBC, imm7  , truncstorei8>;










//===----------------------------------------------------------------------===//
// Non-Instruction Patterns
//===----------------------------------------------------------------------===//


// load immediate
def : Pat<(i32 imm:$imm),
          (LIi (i32 uimm12:$imm) )>;

def : Pat<(i32 imm:$imm),
          (LIl (i32 imm:$imm) )>;

// // jump-table.
// def : Pat<(i32 tjumptable:$dst), (MOV_IMM tjumptable:$dst)>;

// global addresses, external symbols, and frame indices
def : Pat<(gspat:$sym), (LIl gspat:$sym)>;
def : Pat<(espat:$sym), (LIl espat:$sym)>;

def : Pat<(add gspat:$sym, RRegs:$r), (ADDl RRegs:$r, gspat:$sym)>;
def : Pat<(add espat:$sym, RRegs:$r), (ADDl RRegs:$r, espat:$sym)>;

def : Pat<(add gspat:$sym, (shl RRegs:$r, (i32 1))), (SHADDl RRegs:$r, gspat:$sym)>;
def : Pat<(add espat:$sym, (shl RRegs:$r, (i32 1))), (SHADDl RRegs:$r, espat:$sym)>;

def : Pat<(add gspat:$sym, (shl RRegs:$r, (i32 2))), (SHADD2l RRegs:$r, gspat:$sym)>;
def : Pat<(add espat:$sym, (shl RRegs:$r, (i32 2))), (SHADD2l RRegs:$r, espat:$sym)>;

def : Pat<(fipat:$fi) , (ADDi fipat:$fi, 0)>;
def : Pat<(add fipat:$fi, imm7:$imm) , (ADDi fipat:$fi, imm7:$imm)>;
def : Pat<(add fipat:$fi, imm:$imm) , (ADDl fipat:$fi, imm:$imm)>;

def : Pat<(PatmosCall tglobaladdr:$sym), (bs tglobaladdr:$sym)>;
def : Pat<(PatmosCall texternalsym:$sym), (bs texternalsym:$sym)>;

// multiplication
// TODO
//def : Pat<(mul RRegs:$rs1, RRegs:$rs2),
//          (COPY_TO_REGCLASS (MUL RRegs:$rs1, RRegs:$rs2), RRegs)>;


