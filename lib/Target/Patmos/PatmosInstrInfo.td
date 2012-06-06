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
class SDTCisI1 <int OpNum> : SDTCisVT<OpNum,  i1>;
class SDTCisI8 <int OpNum> : SDTCisVT<OpNum,  i8>;
class SDTCisI16<int OpNum> : SDTCisVT<OpNum, i16>;
class SDTCisI32<int OpNum> : SDTCisVT<OpNum, i32>;

//===----------------------------------------------------------------------===//
// Type Profiles.
//===----------------------------------------------------------------------===//

def SDT_PatmosCall         : SDTypeProfile<0, -1, [SDTCisVT<0, iPTR>]>;
def SDT_PatmosCallSeqStart : SDCallSeqStart<[SDTCisI32<0>]>;
def SDT_PatmosCallSeqEnd   : SDCallSeqEnd<[SDTCisI32<0>, SDTCisI32<1>]>;
def SDT_PatmosDynAlloc     : SDTypeProfile<0, 1, [SDTCisI32<0>]>;


//===----------------------------------------------------------------------===//
// Patmos Specific Node Definitions.
//===----------------------------------------------------------------------===//

def PatmosReturn  : SDNode<"PatmosISD::RET_FLAG", SDTNone,
                           [SDNPHasChain, SDNPOptInGlue]>;

def PatmosCall    : SDNode<"PatmosISD::CALL",SDT_PatmosCall,
                           [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                            SDNPVariadic]>;

def PatmosCallseqStart
                  : SDNode<"ISD::CALLSEQ_START", SDT_PatmosCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def PatmosCallseqEnd
                  : SDNode<"ISD::CALLSEQ_END",   SDT_PatmosCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def PatmosMul     : SDNode<"PatmosISD::MUL", SDTIntBinOp,
                           [SDNPCommutative, SDNPAssociative]>;

def PatmosDynAlloc: SDNode<"PatmosISD::DYNALLOC", SDT_PatmosDynAlloc,
                           [SDNPHasChain]>;


//===----------------------------------------------------------------------===//
// Patmos Operand Definitions.
//===----------------------------------------------------------------------===//

def RegisterOperandClass : AsmOperandClass {
  let Name = "Register";
  let ParserMethod = "";
}

// Patmos default predicate operand.
// actually this is a pair of MachineOperands, where
// the first is the predicate register
// and the second is the inversion flag
def pred : PredicateOperand<i1, (ops PRegs:$preg, i1imm:$neg),
                                     (ops (i1 zero_reg), (i1 0))> {
  let PrintMethod = "printPredicateOperand";
  let EncoderMethod = "getPredOperandEncoding";
  let DecoderMethod = "";

}

// Patmos setable predicate operand
// like pred but no default values
def predset : PredicateOperand<i1, (ops PRegs:$preg, i1imm:$neg), (ops )> {
  let PrintMethod = "printPredicateOperand";
  let EncoderMethod = "getPredOperandEncoding";
  let DecoderMethod = "";
}

// for cache relative branches
def brtarget : Operand<OtherVT> {
  //TODO
  //let PrintMethod = "printCacheRelImmOperand";
}


//===----------------------------------------------------------------------===//
// Transformation Functions
//===----------------------------------------------------------------------===//

// Transformation Function - shift right by one/two bits
def shrOne : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getZExtValue() >> 1,
              MVT::i32 );
}]>;
def shrTwo : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getZExtValue() >> 2,
              MVT::i32 );
}]>;


// Transformation Function - count trailing zeros.
def trailingZeros : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getAPIntValue().countTrailingZeros(),
              MVT::i32 );
}]>;

// Transformation Function - count trailing zeros.
def trailingOnes : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getAPIntValue().countTrailingOnes(),
              MVT::i32 );
}]>;




// Transformation Function - negate
def negate : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              -N->getAPIntValue(),
              MVT::i32 );
}]>;

//===----------------------------------------------------------------------===//
// Patmos Complex Pattern Definitions.
//===----------------------------------------------------------------------===//

def predsel    : ComplexPattern<i1, 2, "SelectPred",    [], []>;
def predselinv : ComplexPattern<i1, 2, "SelectPredInv", [], []>;

/*
def addr : ComplexPattern<iPTR, 2, "SelectAddr", [], []>;
*/

//===----------------------------------------------------------------------===//
// Pattern Fragments
//===----------------------------------------------------------------------===//


// offset of indexed load/store
def uimm7    : Operand<i32>, PatLeaf<(imm), [{return isUInt<7>(N->getZExtValue());}]>;
def imm7     : Operand<i32>, PatLeaf<(imm), [{return isInt <7>(N->getSExtValue());}]>;

// ALUi
def uimm12   : Operand<i32>, PatLeaf<(imm), [{return isUInt<12>(N->getZExtValue());}]>;
// Negated unsigned 12 bit value (for load negative short immediate)
def nuimm12  : Operand<i32>, PatLeaf<(imm), [{return isUInt<12>(-N->getSExtValue()); }], negate>;
// Call/Branch/StackCtrl
def uimm22   : Operand<i32>, PatLeaf<(imm), [{return isUInt<22>(N->getZExtValue());}]>;


// 32-bit bitmask transformed to a bit number
def uimm5mask : Operand<i32>, PatLeaf<(imm), [{
 return isPowerOf2_32(N->getZExtValue());
}], trailingZeros>;

// 32-bit inverse bitmask transformed to a bit number
def uimm5imask : Operand<i32>, PatLeaf<(imm), [{
 return isPowerOf2_32(~N->getZExtValue());
}], trailingOnes>;




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

// Helper fragment for condition inversion
def notcc  : PatFrag<(ops node:$in), (i1 (setne node:$in, (i1 -1)))>;









// Patterns to match machine instructions that are more than simple SDNodes

// sub with rhs-lhs
def rsub   :  PatFrag<(ops node:$lhs, node:$rhs),
                      (sub node:$rhs, node:$lhs)>;

// shift-add, for address computations
def shadd  : PatFrag<(ops node:$reg, node:$offs),
                     (add (shl node:$reg, (i32 1)), node:$offs)>;
def shadd2 : PatFrag<(ops node:$reg, node:$offs),
                     (add (shl node:$reg, (i32 2)), node:$offs)>;

def nor    : PatFrag<(ops node:$lhs, node:$rhs),
                     (not (or node:$lhs, node:$rhs))>;


//===----------------------------------------------------------------------===//
// Instruction list..
//===----------------------------------------------------------------------===//


// ADJCALLSTACKDOWN/UP implicitly use/def SP because they may be expanded into
// a stack adjustment and the codegen must know that they may modify the stack
// pointer before prolog-epilog rewriting occurs.
// Pessimistically assume ADJCALLSTACKDOWN / ADJCALLSTACKUP will become
// sub / add which can clobber SP.
let Defs = [RFP, RSP], Uses = [RFP, RSP] in {
def ADJCALLSTACKDOWN : PseudoInst<(outs), (ins i32imm:$amt),
                                    "#ADJCALLSTACKDOWN", "",
                                    [(PatmosCallseqStart timm:$amt)]>;
def ADJCALLSTACKUP   : PseudoInst<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                                    "#ADJCALLSTACKUP", "",
                                    [(PatmosCallseqEnd timm:$amt1, timm:$amt2)]>;
}


//===----------------------------------------------------------------------===//
//  Control Flow Instructions...
//===----------------------------------------------------------------------===//

let isReturn=1, isTerminator=1, isBarrier=1, hasDelaySlot=1,
    Uses = [SB, SO] in {
  def RET  : PFLr<0b0000, (outs), (ins pred:$p),
                  "ret", "", [(PatmosReturn)]>;
}


// isBarrier is cleared later on default predicate
let isTerminator=1, hasDelaySlot=1 in {

  // cache-relative direct branches

  // unconditional
  let isBranch=1, isBarrier=1, isCodeGenOnly=1 in
  def BCu : PFLb<0b01  , (outs), (ins pred:$p, brtarget:$target),
                  "bc", "$target", [(br bb:$target)]>;

  // conditional
  let isBranch=1, isBarrier=0 in
  def BC : PFLb<0b01  , (outs), (ins predset:$p, brtarget:$target),
                "bc", "$target", [(brcond predsel:$p, bb:$target)]>;

  // cache-relative indirect branch
  let isIndirectBranch=1, isBarrier=1 in
  def BCR : PFLi <0b0001, (outs), (ins pred:$p, RRegs:$rs),
                  "bcr", "$rs", [(brind RRegs:$rs)]>;
}




//===----------------------------------------------------------------------===//
//  Call Instructions...
//===----------------------------------------------------------------------===//

let isCall=1, hasDelaySlot=1,
  // Calls may overwrite caller saved registers
  Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16,
          R17, R18, R19, R20,
          SB, SO, SM, SL, SH, S7, S8, S9, S10, S11, S12, S13, S14, S15] in {

  def bs : PFLb<0b00  , (outs), (ins pred:$p, uimm22:$imm),
                    "bs", "$imm",
                    [(PatmosCall uimm22:$imm)]>;

  def bsr: PFLi<0b0000, (outs), (ins pred:$p, RRegs:$rs1),
                    "bsr", "$rs1",
                    [(PatmosCall RRegs:$rs1)]>;
}

//===----------------------------------------------------------------------===//
//  Miscellaneous Instructions...
//===----------------------------------------------------------------------===//

def SRES : STC<0b00, (outs), (ins pred:$p, uimm22:$imm), "sres", "$imm",
               []>;
def SENS : STC<0b01, (outs), (ins pred:$p, uimm22:$imm), "sens", "$imm",
               []>;
def SFREE: STC<0b10, (outs), (ins pred:$p, uimm22:$imm), "sfree", "$imm", []>;

//===----------------------------------------------------------------------===//
// Move Instructions
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
// Arithmetic Instructions
//===----------------------------------------------------------------------===//


// no short immediates
multiclass BinArithLR<bits<4> func, string asmop, SDNode opnode, bit isrCommutable=0> {
  def l : ALUl <func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
                asmop, "$rd = $rs1, $imm",
                [(set RRegs:$rd, (opnode RRegs:$rs1, (i32 imm:$imm)))]>;

  let isCommutable = isrCommutable in
  def r : ALUr <func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
                asmop, "$rd = $rs1, $rs2",
                [(set RRegs:$rd, (opnode RRegs:$rs1, RRegs:$rs2))]>;
}

// also support short immediates
multiclass BinArithILR<bits<4> func, string asmop, SDNode opnode, bit isrCommutable=0> {
  def i : ALUi<func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, uimm12:$imm),
               asmop, "$rd = $rs1, $imm",
               [(set RRegs:$rd, (opnode RRegs:$rs1, (i32 uimm12:$imm)))]>;
  // also L and R
  defm NAME : BinArithLR<func, asmop, opnode, isrCommutable>;
}

// same as above with pattern fragments instead of simple sdnodes
multiclass BinArithLRpfg<bits<4> func, string asmop, PatFrag pfg, bit isrCommutable=0> {
  def l : ALUl<func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, i32imm:$imm),
               asmop, "$rd = $rs1, $imm",
               [(set RRegs:$rd, (pfg RRegs:$rs1, imm:$imm))]>;

  let isCommutable = isrCommutable in
  def r : ALUr<func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
               asmop, "$rd = $rs1, $rs2",
               [(set RRegs:$rd, (pfg RRegs:$rs1, RRegs:$rs2))]>;
}

multiclass BinArithILRpfg<bits<4> func, string asmop, PatFrag pfg, bit isrCommutable=0> {
  def i : ALUi<func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, uimm12:$imm),
               asmop, "$rd = $rs1, $imm",
               [(set RRegs:$rd, (pfg RRegs:$rs1, uimm12:$imm))]>;
  // also L and R
  defm NAME : BinArithLRpfg<func, asmop, pfg>;
}


// Binary Arithmetic instructions

// with short immediates:
defm ADD    : BinArithILR   <0b0000, "add"   , add, 1>;
defm SUB    : BinArithILR   <0b0001, "sub"   , sub>;
defm RSUB   : BinArithILRpfg<0b0010, "rsub"  , rsub>;
defm SL     : BinArithILR   <0b0011, "sl"    , shl>; // FIXME 5 bits
defm SR     : BinArithILR   <0b0100, "sr"    , srl>; // FIXME 5 bits
defm SRA    : BinArithILR   <0b0101, "sra"   , sra>; // FIXME 5 bits
defm OR     : BinArithILR   <0b0110, "or"    , or, 1>;
defm AND    : BinArithILR   <0b0111, "and"   , and, 1>;
// no short immediates:
defm RL     : BinArithLR    <0b1000, "rl"    , rotl>; // FIXME 5 bits
defm RR     : BinArithLR    <0b1001, "rr"    , rotr>; // FIXME 5 bits
defm XOR    : BinArithLR    <0b1010, "xor"   , xor, 1>;
defm NOR    : BinArithLRpfg <0b1011, "nor"   , nor, 1>;
defm SHADD  : BinArithLRpfg <0b1100, "shadd ", shadd>;
defm SHADD2 : BinArithLRpfg <0b1101, "shadd2", shadd2>;


// Pseudo Instructions using binary arithmetic opcodes


// Allocate space on the current stack space -- aka alloca
let Defs = [RSP], Uses = [RSP] in
def DYNALLOC : PatmosInst<(outs RRegs:$rd),
                          (ins pred:$p, RRegs:$rs1, uimm12:$imm),
                          "add", "$rd = $rs1, $imm # dynalloc",
                          [], FOther, IIC_ALU>;


// move instructions
let isAsCheapAsAMove=1, Imm12=0 in {
  // mov Rd = Rs ... add Rd = Rs + 0
  def MOV : ALUi<0b0000, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
                 "mov", "$rd = $rs", []>;

  // conditional move, like mov with setable predicate
  // $rs1 = $rd, only if the condition in $p is true, $r2 is written to $rd thus overwriting $rs1
  let Constraints = "$old = $rd", isCodeGenOnly = 1 in
  def CMOV : ALUi<0b0000, (outs RRegs:$rd), (ins predset:$p, RRegs:$old, RRegs:$new),
                  "mov", "$rd = $new",
                  [(set RRegs:$rd, (select predsel:$p, RRegs:$new, RRegs:$old))]>;
}

// neg Rd = -Rs ... rsub Rd = 0 - Rs
let Imm12 = 0 in
def NEG : ALUi<0b0010, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
               "neg", "$rd = $rs",
               [(set RRegs:$rd, (ineg RRegs:$rs))]>;


// not Rd = ~Rs ... nor Rd = ~(Rs | R0)
let Rs2 = 0 in
def NOT : ALUr<0b1011, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
               "not", "$rd = $rs",
               [(set RRegs:$rd, (not RRegs:$rs))]>;


// zext8 Rd = (uin8_t)Rs ... and Rd = Rs & 0xff
let Imm12 = 0xff in
def ZEXT8 : ALUi <0b0111,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
              "zext8", "$rd = $rs",
              [(set RRegs:$rd, (and RRegs:$rs, (i32 0xff)))]>;



let Rs1 = 0,
    isAsCheapAsAMove = 1,
    isMoveImm = 1
in {
  let isReMaterializable = 1 in {
    // li Rd = Immedate ... add Rd = r0 + Immediate (short, positive)
    def LIi  : ALUi<0b0000, (outs RRegs:$rd), (ins pred:$p, uimm12:$imm),
                    "li", "$rd = $imm",
                    [(set RRegs:$rd, (add R0, uimm12:$imm))]>;
    // li Rd = Immedate ... sub Rd = r0 - Immediate (short, negative)
    def LIin : ALUi<0b0001, (outs RRegs:$rd), (ins pred:$p, nuimm12:$imm),
                    "li", "$rd = $imm",
                    [(set RRegs:$rd, (sub R0, nuimm12:$imm))]>;
    // li Rd = Immedate ... add Rd = r0 + Immediate (long immediate)
    def LIl  : ALUl<0b0000, (outs RRegs:$rd), (ins pred:$p, i32imm:$imm),
                    "li", "$rd = $imm",
                    [(set RRegs:$rd, (add R0, (i32 imm:$imm)))]>;
  }
  // conditional load immediates
  let isCodeGenOnly = 1 in {
    def CLIi  : ALUi<0b0000, (outs RRegs:$rd), (ins predset:$p, uimm12:$imm),
		     "li", "$rd = $imm", []>;
    def CLIin : ALUi<0b0001, (outs RRegs:$rd), (ins predset:$p, nuimm12:$imm),
		     "li", "$rd = $imm", []>;
    def CLIl  : ALUl<0b0000, (outs RRegs:$rd), (ins predset:$p, i32imm:$imm),
		     "li", "$rd = $imm", []>;
  }
}



/********/
// Unary Arithmetic

def SEXT8  : ALUu <0b0000,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
              "sext8", "$rd = $rs",
              [(set RRegs:$rd, (sext_inreg RRegs:$rs, i8))]>;

def SEXT16 : ALUu <0b0001,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
               "sext16", "$rd = $rs",
              [(set RRegs:$rd, (sext_inreg RRegs:$rs, i16))]>;

def ZEXT16 : ALUu <0b0010,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
              "zext16", "$rd = $rs",
              [(set RRegs:$rd, (and RRegs:$rs, (i32 0xffff)))]>;


// TODO experimental: other lowerings?
// TODO does the add overflow do any harm?
def ABS    : ALUu <0b0011,
              (outs RRegs:$rd), (ins pred:$p, RRegs:$rs),
              "abs", "$rd = $rs",
              [(set RRegs:$rd, (xor (add RRegs:$rs, (sra RRegs:$rs, (i32 31))), (sra RRegs:$rs, (i32 31))))]>;





// Multiply

// TODO
/*
let Defs = [SL,SH], isCommutable=1 in {
def MUL  : ALUm <0b0000,
              (outs Mul:$sd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
              "mul", "$$$sd, $rs1, $rs2",
              [(set Mul:$sd, (patmos_mul RRegs:$rs1, RRegs:$rs2))]>;

def MULU : ALUm <0b0001,
              (outs), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
              "mulu", "$rs1, $rs2",
              []>;
              //[(set SRegs:$sl, (mul RRegs:$rs1, RRegs:$rs2))]>;
}
*/

// Compare

class Compare<string asmop, bits<4> opcode>
      : ALUc <opcode,
              (outs PRegs:$pd), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
              asmop, "$pd = $rs1, $rs2", []>;

def CMPEQ  : Compare <"cmpeq" , 0b0000>;
def CMPNEQ : Compare <"cmpneq", 0b0001>;
def CMPLT  : Compare <"cmplt" , 0b0010>;
def CMPLE  : Compare <"cmple" , 0b0011>;
def CMPULT : Compare <"cmpult", 0b0100>;
def CMPULE : Compare <"cmpule", 0b0101>;
def BTEST  : Compare <"btest" , 0b0110>;

// Pseudo instr ISODD uses btest opcode
let Rs2 = 0 in
def ISODD : ALUc <0b0110, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs),
                  "isodd", "$pd = $rs",
                  [(set PRegs:$pd, (setne (and RRegs:$rs, (i32 1)), (i32 1)))]>;

// Pseudo mov Pd <- Rs uses cmpneq with r0
let Rs2 = 0 in
def MOVrp : ALUc <0b0001, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs),
                  "mov", "$pd = $rs",
                  [(set PRegs:$pd, (setne RRegs:$rs, (i32 0)))]>;



// Predicate Combine


class PredCombine<bits<4> opcode, string asmop>
      : ALUp <opcode,
              (outs PRegs:$pd), (ins pred:$p, predset:$ps1, predset:$ps2),
              asmop, "$pd = $ps1, $ps2", []>;

def POR:  PredCombine <0b0110, "or">;
def PAND: PredCombine <0b0111, "and">;
def PXOR: PredCombine <0b1010, "xor">;
def PNOR: PredCombine <0b1011, "nor">;


// Pseudos:
//mov uses or opcode (or Pd = Ps | !p0)
let Ps2 = 0b1000 in
def PMOV: ALUp <0b0110, (outs PRegs:$pd), (ins pred:$p, predset:$ps),
                "mov", "$pd = $ps", []>;

//not uses nor opcode (nor Pd = Ps ^ p0)
let Ps2 = 0 in
def PNOT: ALUp <0b1011, (outs PRegs:$pd), (ins pred:$p, predset:$ps),
                "not", "$pd = $ps",
                [(set PRegs:$pd, (not predsel:$ps))]>;

//set uses or opcode (or Pd = p0 | p0)
let Ps1 = 0, Ps2 = 0 in
def PSET: ALUp <0b0110, (outs PRegs:$pd), (ins pred:$p),
                "set", "$pd",
                [(set PRegs:$pd, (i1 1))]>;

//clr uses nor opcode (nor Pd = p0 ^ p0)
let Ps1 = 0, Ps2 = 0 in
def PCLR: ALUp <0b1010, (outs PRegs:$pd), (ins pred:$p),
                "clr", "$pd",
                [(set PRegs:$pd, (i1 0))]>;


// NOP
let neverHasSideEffects = 1 in
def NOP  : SPCn <(outs), (ins pred:$p, i32imm:$imm), "nop", "$imm", []>;

// Wait
def WAIT : SPCw <0b0000, (outs), (ins pred:$p), "wait.mem", "", []>;

// move to special
def MTS  : SPCt <(outs SRegs:$sd), (ins pred:$p, RRegs:$rs), "mts", "$sd = $rs", [] >;

// move from special
def MFS  : SPCf <(outs RRegs:$rd), (ins pred:$p, SRegs:$ss), "mfs", "$rd = $ss", [] >;



// Load typed

multiclass LoadTyped<bits<3> ty, Operand immTy, string asmop>
{
  // load from stack cache
  def S  : LDT<ty, 0b0, 0b0, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               asmop # "s", "$rd = [${ra:addrmod}${imm:addrmod}]",
               [] >;

  // load from local memory
  def L  : LDT<ty, 0b0, 0b1, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               asmop # "l", "$rd = [${ra:addrmod}${imm:addrmod}]",
               [] >;

  // load from data cache
  def C  : LDT<ty, 0b1, 0b0, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               asmop # "c", "$rd = [${ra:addrmod}${imm:addrmod}]",
               [] >;

  // load from main memory
  def M  : LDT<ty, 0b1, 0b1, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               asmop # "m", "$rd = [${ra:addrmod}${imm:addrmod}]",
               [] >;
}

multiclass DecoupledLoadTyped<bits<4> ty, Operand immTy, string asmop>
{
  let Defs = [SM], Rd = 0 in
  {
    // load from data cache (no pattern to match)
    def C  : LDT<ty{3-1}, ty{0}, 0b0, (outs /*empty*/),
                 (ins pred:$p, RRegs:$ra, immTy:$imm),
                 "d" # asmop # "c", "sm = [${ra:addrmod}${imm:addrmod}]",
                 [] >;

    // load from main memory (no pattern to match)
    def M  : LDT<ty{3-1}, ty{0}, 0b1, (outs /*empty*/),
                 (ins pred:$p, RRegs:$ra, immTy:$imm),
                 "d" # asmop # "m", "sm = [${ra:addrmod}${imm:addrmod}]",
                 [] >;
  }
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



// Store typed

multiclass StoreTyped<bits<3> ty, Operand immTy, string asmop>
{
  // store to stack cache
  def S  : STT<ty, 0b00, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               asmop # "s", "[${ra:addrmod}${imm:addrmod}] = $rs",
               [] >;

  // store to local memory
  def L  : STT<ty, 0b01, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               asmop # "l", "[${ra:addrmod}${imm:addrmod}] = $rs",
               [] >;

  // store to data cache
  def C  : STT<ty, 0b10, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               asmop # "c", "[${ra:addrmod}${imm:addrmod}] = $rs",
               [] >;

  // store to stack cache
  def M  : STT<ty, 0b11, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               asmop # "m", "[${ra:addrmod}${imm:addrmod}] = $rs",
               [] >;
}


defm SW  : StoreTyped<0b000, imm7s2, "sw">;
defm SH  : StoreTyped<0b001, imm7s1, "sh">;
defm SB  : StoreTyped<0b010, imm7  , "sb">;


//===----------------------------------------------------------------------===//
// Non-Instruction Patterns
//===----------------------------------------------------------------------===//

// dynamic stack allocation (aka alloca)
def : Pat<(PatmosDynAlloc (i32 0)), (DYNALLOC RSP, 0)>;


// Patterns are in a separate file
include "PatmosInstrPatterns.td"


