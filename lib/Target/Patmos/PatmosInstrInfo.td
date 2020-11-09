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

def SDT_PatmosMul          : SDTypeProfile<0, 2, [SDTCisInt<0>,
                                                  SDTCisSameAs<0, 1>]>;

def SDTBrjt                : SDTypeProfile<0, 2, [SDTCisPtrTy<0>,
                                                  SDTCisSameAs<0, 1> ]>;

//===----------------------------------------------------------------------===//
// Patmos Specific Predicates
//===----------------------------------------------------------------------===//
def NotLargeCode : Predicate<"TM.getCodeModel() != CodeModel::Large">;

//===----------------------------------------------------------------------===//
// Patmos Specific Node Definitions.
//===----------------------------------------------------------------------===//

def PatmosReturn  : SDNode<"PatmosISD::RET_FLAG", SDTNone,
                           [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def PatmosCall    : SDNode<"PatmosISD::CALL", SDT_PatmosCall,
                           [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                            SDNPVariadic, SDNPMemOperand]>;

def PatmosCallseqStart
                  : SDNode<"ISD::CALLSEQ_START", SDT_PatmosCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def PatmosCallseqEnd
                  : SDNode<"ISD::CALLSEQ_END",   SDT_PatmosCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def PatmosMul     : SDNode<"PatmosISD::MUL", SDT_PatmosMul,
                           [SDNPCommutative, SDNPAssociative, SDNPOutGlue]>;

def PatmosMulU    : SDNode<"PatmosISD::MULU", SDT_PatmosMul,
                           [SDNPCommutative, SDNPAssociative, SDNPOutGlue]>;

def brjt          : SDNode<"ISD::BR_JT", SDTBrjt,  [SDNPHasChain]>;

//===----------------------------------------------------------------------===//
// Patmos Operand Definitions.
//===----------------------------------------------------------------------===//

//def RegisterOperandClass : AsmOperandClass {
//  let Name = "Register";
//  let ParserMethod = "";
//}

// Patmos default predicate operand.
// actually this is a pair of MachineOperands, where
// the first is the predicate register
// and the second is the inversion flag
def pred : PredicateOperand<i1, (ops PRegs:$preg, i1imm:$neg),
                                     (ops (i1 zero_reg), (i1 0))> {
  let PrintMethod = "printPredicateOperand";
  let EncoderMethod = "getPredOperandEncoding";
  let DecoderMethod = "DecodePredRegisterClass";

}

// Patmos setable predicate operand
// like pred but no default values
def predset : PredicateOperand<i1, (ops PRegs:$preg, i1imm:$neg), (ops )> {
  let PrintMethod = "printPredicateOperand";
  let EncoderMethod = "getPredOperandEncoding";
  let DecoderMethod = "DecodePredRegisterClass";
}

// for pc relative branches
def pcreltarget : Operand<OtherVT> {
  let PrintMethod = "printPCRelTargetOperand";
}


//===----------------------------------------------------------------------===//
// Transformation Functions
//===----------------------------------------------------------------------===//

// Transformation Function - shift right by one/two bits
def shrOne : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getSExtValue() >> 1,
              MVT::i32 );
}]>;
def shrTwo : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getSExtValue() >> 2,
              MVT::i32 );
}]>;

// Transformation Function - mask low 7/8/9 bits
def maskSeven : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getSExtValue() & (-1 << 7),
              MVT::i32 );
}]>;
def maskEight : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getSExtValue() & (-1 << 8),
              MVT::i32 );
}]>;
def maskNine : SDNodeXForm<imm, [{
 return CurDAG->getTargetConstant(
              N->getSExtValue() & (-1 << 9),
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
def imm7     : Operand<i32>, PatLeaf<(imm), [{return isInt <7>(N->getZExtValue());}]>;

// ALUi
def uimm12   : Operand<i32>, PatLeaf<(imm), [{return isUInt<12>(N->getZExtValue());}]>;
// Negated unsigned 12 bit value (for load negative short immediate)
def nuimm12  : Operand<i32>, PatLeaf<(imm), [{return isUInt<12>(-N->getSExtValue()); }], negate>;
// StackCtrl
def uimm18   : Operand<i32>, PatLeaf<(imm), [{return isUInt<18>(N->getZExtValue());}]>;
// Call/Branch
def uimm22   : Operand<i32>, PatLeaf<(imm), [{return isUInt<22>(N->getZExtValue());}]>;
def imm22    : Operand<i32>, PatLeaf<(imm), [{return isInt <22>(N->getSExtValue());}]>;
// Compare immediate, Trap
def uimm5    : Operand<i32>, PatLeaf<(imm), [{return isUInt<5>(N->getZExtValue());}]>;

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
// 18 bit value, shifted by two bits (for STC)
def uimm18s2 : Operand<i32>, PatLeaf<(imm), [{
 return isUInt<18>(N->getZExtValue() >> 2) && ((N->getZExtValue() & 0x03) == 0);
}], shrTwo>;
// 22 bit value, shifted by two bits (for direct calls and branches)
def uimm22s2 : Operand<i32>, PatLeaf<(imm), [{
 return isUInt<22>(N->getZExtValue() >> 2) && ((N->getZExtValue() & 0x03) == 0);
}], shrTwo>;
def imm22s2  : Operand<i32>, PatLeaf<(imm), [{
 return isInt<22>(N->getSExtValue() >> 2) && ((N->getSExtValue() & 0x03) == 0);
}], shrTwo>;


// Transformation for baseImm+shortImm addressing
def immBase7   : Operand<i32>, PatLeaf<(imm), [{}], maskSeven>;
def immBase7s1 : Operand<i32>, PatLeaf<(imm), [{}], maskEight>;
def immBase7s2 : Operand<i32>, PatLeaf<(imm), [{}], maskNine>;


// Transform all FrameIndex objects into TargetFrameIndex objects and define a
// pattern that allows us to match against FrameIndex patterns
def FI_XForm : SDNodeXForm<frameindex, [{ return CurDAG->getTargetFrameIndex(N->getIndex(), N->getValueType(0)); }]>;
def fipat : PatLeaf<(i32 frameindex), [{}], FI_XForm>;

def JT_XForm : SDNodeXForm<jumptable, [{ return CurDAG->getTargetJumpTable(N->getIndex(), N->getValueType(0), N->getTargetFlags()); }]>;
def jtpat : Operand<i32>, PatLeaf<(i32 jumptable), [{}], JT_XForm>;

def GS_XForm : SDNodeXForm<globaladdr, [{ return CurDAG->getTargetGlobalAddress(N->getGlobal(), N, N->getValueType(0), N->getOffset(), N->getTargetFlags()); }]>;
def gspat : PatLeaf<(i32 globaladdr), [{}], GS_XForm>;

def ES_XForm : SDNodeXForm<externalsym, [{ return CurDAG->getTargetExternalSymbol(N->getSymbol(), N->getValueType(0), N->getTargetFlags()); }]>;
def espat : PatLeaf<(i32 externalsym), [{}], ES_XForm>;

// Helper fragment for condition inversion
def notcc  : PatFrag<(ops node:$in), (i1 (setne node:$in, (i1 -1)))>;









// Patterns to match machine instructions that are more than simple SDNodes

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


let mayStore=1 in
def PSEUDO_PREG_SPILL  : PseudoInst<(outs),
                                    (ins RRegs:$ra, i32imm:$imm, PRegs:$ps),
                                    "#PSEUDO_PREG_SPILL", "", []>;

let mayLoad=1 in
def PSEUDO_PREG_RELOAD : PseudoInst<(outs PRegs:$pd),
                                    (ins RRegs:$ra, i32imm:$imm),
                                    "#PSEUDO_PREG_RELOAD", "", []>;

def PSEUDO_LOOPBOUND : PseudoInst<(outs), (ins i32imm:$minv, i32imm:$maxv),
                          "#PSEUDO_LOOPBOUND", "[$minv,$maxv]",
                          [(loopbound (i32 imm:$minv), (i32 imm:$maxv))]>;



//===----------------------------------------------------------------------===//
//  Control Flow Instructions...
//===----------------------------------------------------------------------===//


let isTerminator=1, isBranch=1, hasDelaySlot=1, ImmSigned=1 in {

  // NOTE: This has to be kept consistent with HasPCRELImmediate in PatmosInstrInfo.h

  // local branches

  // unconditional
  let isBarrier=1, isCodeGenOnly=1, Defs=[RTR] in
  def BRu : CFLi<0b01, 0b1, (outs), (ins pred:$p, pcreltarget:$target),
                 "br", "$target", [(br bb:$target)]>;

  // conditional
  let isBarrier=0, Defs=[RTR] in
  def BR  : CFLi<0b01, 0b1, (outs), (ins predset:$p, pcreltarget:$target),
                 "br", "$target", [(brcond predsel:$p, bb:$target)]>;

  // indirect-unconditional
  let isIndirectBranch=1, isBarrier=1, isCodeGenOnly=1 in
  def BRRu: CFLrs<0b01, 0b1, (outs), (ins pred:$p, RRegs:$rs),
                  "br", "$rs", [(brind RRegs:$rs)]>;

  // indirect-conditional
  let isIndirectBranch=1, isBarrier=0, hasSideEffects = 0 in
  def BRR : CFLrs<0b01, 0b1, (outs), (ins pred:$p, RRegs:$rs),
                  "br", "$rs", []>;

  // jumptable-unconditional
  let isIndirectBranch=1, isBarrier=1, isCodeGenOnly=1,
      hasSideEffects = 0 in
  def BRTu: CFLrs<0b01, 0b1, (outs), (ins pred:$p, RRegs:$rs, jtpat:$jtidx),
                  "br", "$rs", []>;

  // jumptable-conditional
  let isIndirectBranch=1, isBarrier=0, isCodeGenOnly=1,
      hasSideEffects = 0 in
  def BRT : CFLrs<0b01, 0b1, (outs), (ins pred:$p, RRegs:$rs, jtpat:$jtidx),
                  "br", "$rs", []>;


  // branches with cache-fill

  let hasSideEffects = 0, mayStall = 1 in { // due to lacking pattern
    // unconditional
    let isBarrier=1, isCodeGenOnly=1 in
    def BRCFu : CFLi<0b10, 0b1, (outs), (ins pred:$p, uimm22s2:$target),
                     "brcf", "$target", []>;

    // conditional
    let isBarrier=0 in
    def BRCF  : CFLi<0b10, 0b1, (outs), (ins predset:$p, uimm22s2:$target),
                     "brcf", "$target", []>;

    // indirect-unconditional
    let isIndirectBranch=1, isBarrier=1, isCodeGenOnly=1, rs2 = 0 in
    def BRCFRu: CFLrt<0b10, 0b1, (outs), (ins pred:$p, RRegs:$rs1),
                      "brcf", "$rs1", []>;

    // indirect-conditional
    let isIndirectBranch=1, isBarrier=0, rs2 = 0 in
    def BRCFR : CFLrt<0b10, 0b1, (outs), (ins pred:$p, RRegs:$rs1),
                      "brcf", "$rs1", []>;

    // jumptable-unconditional
    let isIndirectBranch=1, isBarrier=1, isCodeGenOnly=1, rs2 = 0 in
    def BRCFTu: CFLrt<0b10, 0b1, (outs), (ins pred:$p, RRegs:$rs1, jtpat:$jtidx),
                      "brcf", "$rs1", []>;

    // jumptable-conditional
    let isIndirectBranch=1, isBarrier=0, isCodeGenOnly=1, rs2 = 0 in
    def BRCFT : CFLrt<0b10, 0b1, (outs), (ins pred:$p, RRegs:$rs1, jtpat:$jtidx),
                      "brcf", "$rs1", []>;
  }
}

// non-delayed branches
let isTerminator=1, isBranch=1, hasDelaySlot=0, ImmSigned=1 in {

  // NOTE: This has to be kept consistent with HasPCRELImmediate in PatmosInstrInfo.h

  // local branches

  // unconditional
  let isBarrier=1, isCodeGenOnly=1, Defs=[RTR] in
  def BRNDu : CFLi<0b01, 0b0, (outs), (ins pred:$p, pcreltarget:$target),
                 "brnd", "$target", [(br bb:$target)]>;

  // conditional
  let isBarrier=0, Defs=[RTR] in
  def BRND  : CFLi<0b01, 0b0, (outs), (ins predset:$p, pcreltarget:$target),
                 "brnd", "$target", [(brcond predsel:$p, bb:$target)]>;

  // indirect-unconditional
  let isIndirectBranch=1, isBarrier=1, isCodeGenOnly=1 in
  def BRRNDu: CFLrs<0b01, 0b0, (outs), (ins pred:$p, RRegs:$rs),
                     "brnd", "$rs", [(brind RRegs:$rs)]>;

  // indirect-conditional
  let isIndirectBranch=1, isBarrier=0, hasSideEffects = 0 in
  def BRRND : CFLrs<0b01, 0b0, (outs), (ins pred:$p, RRegs:$rs),
                   "brnd", "$rs", []>;

  // jumptable-unconditional
  let isIndirectBranch=1, isBarrier=1, isCodeGenOnly=1,
      hasSideEffects = 0 in
  def BRTNDu: CFLrs<0b01, 0b0, (outs), (ins pred:$p, RRegs:$rs, jtpat:$jtidx),
                     "brnd", "$rs", []>;

  // jumptable-conditional
  let isIndirectBranch=1, isBarrier=0, isCodeGenOnly=1,
      hasSideEffects = 0 in
  def BRTND : CFLrs<0b01, 0b0, (outs), (ins pred:$p, RRegs:$rs, jtpat:$jtidx),
                   "brnd", "$rs", []>;

  let hasSideEffects = 0, mayStall = 1 in { // due to lacking pattern
    // unconditional
    let isBarrier=1, isCodeGenOnly=1 in
    def BRCFNDu : CFLi<0b10, 0b0, (outs), (ins pred:$p, uimm22s2:$target),
                       "brcfnd", "$target", []>;

    // conditional
    let isBarrier=0 in
    def BRCFND  : CFLi<0b10, 0b0, (outs), (ins predset:$p, uimm22s2:$target),
                       "brcfnd", "$target", []>;

    // indirect-unconditional
    let isIndirectBranch=1, isBarrier=1, isCodeGenOnly=1, rs2 = 0 in
    def BRCFRNDu: CFLrt<0b10, 0b0, (outs), (ins pred:$p, RRegs:$rs1),
                         "brcfnd", "$rs1", []>;

    // indirect-conditional
    let isIndirectBranch=1, isBarrier=0, rs2 = 0 in
    def BRCFRND : CFLrt<0b10, 0b0, (outs), (ins pred:$p, RRegs:$rs1),
                       "brcfnd", "$rs1", []>;

    // jumptable-unconditional
    let isIndirectBranch=1, isBarrier=1, isCodeGenOnly=1, rs2 = 0 in
    def BRCFTNDu: CFLrt<0b10, 0b0, (outs), (ins pred:$p, RRegs:$rs1, jtpat:$jtidx),
                         "brcfnd", "$rs1", []>;

    // jumptable-conditional
    let isIndirectBranch=1, isBarrier=0, isCodeGenOnly=1, rs2 = 0 in
    def BRCFTND : CFLrt<0b10, 0b0, (outs), (ins pred:$p, RRegs:$rs1, jtpat:$jtidx),
                       "brcfnd", "$rs1", []>;
  }
}

//===----------------------------------------------------------------------===//
//  Call Instructions...
//===----------------------------------------------------------------------===//

let isCall=1, hasDelaySlot=1, mayStall=1,
  // Calls may overwrite caller saved registers
  Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16,
          R17, R18, R19, R20,
          SL, SH, SRB, SRO, SXB, SXO, S11, S12, S13, S14, S15] in {

  // NOTE: This has to be kept consistent with HasPCRELImmediate in PatmosInstrInfo.h

  def CALL : CFLi<0b00, 0b1, (outs), (ins pred:$p, uimm22s2:$target),
                  "call", "$target", [(PatmosCall uimm22s2:$target)]>;

  def CALLR: CFLrs<0b00, 0b1, (outs), (ins pred:$p, RRegs:$rs),
                   "call", "$rs", [(PatmosCall RRegs:$rs)]>;
}

let isCall=1, hasDelaySlot=0, mayStall=1,
  // Calls may overwrite caller saved registers
  Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16,
          R17, R18, R19, R20,
          SL, SH, SRB, SRO, SXB, SXO, S11, S12, S13, S14, S15] in {

  // NOTE: This has to be kept consistent with HasPCRELImmediate in PatmosInstrInfo.h

  def CALLND : CFLi<0b00, 0b0, (outs), (ins pred:$p, uimm22s2:$target),
                  "callnd", "$target", [(PatmosCall uimm22s2:$target)]>;

  def CALLRND : CFLrs<0b00, 0b0, (outs), (ins pred:$p, RRegs:$rs),
                     "callnd", "$rs", [(PatmosCall RRegs:$rs)]>;
}

let isReturn=1, isTerminator=1, isBarrier=1, hasDelaySlot=1, mayStall=1,
    hasExtraSrcRegAllocReq = 1, hasSideEffects = 0 in { // due to missing pattern
  let  Uses = [SRB, SRO] in
    def RET  : CFLri<0b00, 0b1, (outs), (ins pred:$p),
                     "ret", "", []>;
  let  Uses = [SXB, SXO] in
    def XRET : CFLri<0b01, 0b1, (outs), (ins pred:$p),
                     "xret", "", []>;
}

let isReturn=1, isTerminator=1, isBarrier=1, hasDelaySlot=0, mayStall=1,
    hasSideEffects = 0 in { // due to missing pattern
  let  Uses = [SRB, SRO] in
    def RETND  : CFLri<0b00, 0b0, (outs), (ins pred:$p),
                      "retnd", "", []>;
  let  Uses = [SXB, SXO] in
    def XRETND : CFLri<0b01, 0b0, (outs), (ins pred:$p),
                       "xretnd", "", []>;
}

let isCall=1, hasDelaySlot=0,
  // Traps are like calls and may overwrite caller saved registers
  Defs = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16,
          R17, R18, R19, R20,
          SL, SH, SRB, SRO, SXB, SXO, S11, S12, S13, S14, S15] in {
  def TRAP : CFLi<0b11, 0b0, (outs), (ins pred:$p, uimm5:$target),
  	  	   	 	  "trap", "$target", []>;
}	

//===----------------------------------------------------------------------===//
//  Miscellaneous Instructions...
//===----------------------------------------------------------------------===//

let Defs = [SS, ST], Uses = [SS, ST] in {
  def SRESi  : STCi<0b00, (outs), (ins pred:$p, uimm18s2:$imm), "sres",   "$imm", []>;
  def SENSi  : STCi<0b01, (outs), (ins pred:$p, uimm18s2:$imm), "sens",   "$imm", []>;
  def SFREEi : STCi<0b10, (outs), (ins pred:$p, uimm18s2:$imm), "sfree",  "$imm", []>;
  def SSPILLi: STCi<0b11, (outs), (ins pred:$p, uimm18s2:$imm), "sspill", "$imm", []>;
  def SENSr  : STCr<0b01, (outs), (ins pred:$p, RRegs:$rs), "sens",   "$rs", []>;
  def SSPILLr: STCr<0b11, (outs), (ins pred:$p, RRegs:$rs), "sspill", "$rs", []>;
}

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

  // overwrite variants
  let Constraints = "$rold = $rd", isCodeGenOnly=1, hasSideEffects = 0 in {
    def l_ow : ALUl<func, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, i32imm:$imm, RRegs:$rold),
                 asmop, "$rd = $rs1, $imm", []>;

    let isCommutable = isrCommutable in
    def r_ow : ALUr<func, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, RRegs:$rs2, RRegs:$rold),
                 asmop, "$rd = $rs1, $rs2", []>;
  }
}

// also support short immediates
multiclass BinArithILR<bits<4> func, string asmop, SDNode opnode, bit isrCommutable=0> {
  def i : ALUi<func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, uimm12:$imm),
               asmop, "$rd = $rs1, $imm",
               [(set RRegs:$rd, (opnode RRegs:$rs1, (i32 uimm12:$imm)))]>;

  let Constraints = "$rold = $rd", isCodeGenOnly=1, hasSideEffects = 0 in
  def i_ow : ALUi<func, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, uimm12:$imm, RRegs:$rold),
               asmop, "$rd = $rs1, $imm", []>;
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

  // overwrite variants
  let Constraints = "$rold = $rd", isCodeGenOnly=1, hasSideEffects = 0 in {
    def l_ow : ALUl<func, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, i32imm:$imm, RRegs:$rold),
                 asmop, "$rd = $rs1, $imm", []>;

    let isCommutable = isrCommutable in
    def r_ow : ALUr<func, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, RRegs:$rs2, RRegs:$rold),
                 asmop, "$rd = $rs1, $rs2", []>;
  }
}

multiclass BinArithILRpfg<bits<4> func, string asmop, PatFrag pfg, bit isrCommutable=0> {
  def i : ALUi<func, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, uimm12:$imm),
               asmop, "$rd = $rs1, $imm",
               [(set RRegs:$rd, (pfg RRegs:$rs1, uimm12:$imm))]>;

  let Constraints = "$rold = $rd", isCodeGenOnly=1, hasSideEffects = 0 in
  def i_ow : ALUi<func, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, uimm12:$imm, RRegs:$rold),
               asmop, "$rd = $rs1, $imm", []>;
  // also L and R
  defm NAME : BinArithLRpfg<func, asmop, pfg>;
}


// Binary Arithmetic instructions

// NOTE: the list of instructions (including pseudos) that have ALUi and ALUl format must be
// kept consistent with HasALUlVariant() in PatmosInstrInfo.h

// with short immediates:
// dont forget to update PatmosInstrInfo.h:276
defm ADD    : BinArithILR   <0b0000, "add"   , add, 1>;
defm SUB    : BinArithILR   <0b0001, "sub"   , sub>;
defm XOR    : BinArithILR   <0b0010, "xor"   , xor, 1>;
defm SL     : BinArithILR   <0b0011, "sl"    , shl>; // FIXME 5 bits
defm SR     : BinArithILR   <0b0100, "sr"    , srl>; // FIXME 5 bits
defm SRA    : BinArithILR   <0b0101, "sra"   , sra>; // FIXME 5 bits
defm OR     : BinArithILR   <0b0110, "or"    , or,  1>;
defm AND    : BinArithILR   <0b0111, "and"   , and, 1>;
// no short immediates:
defm NOR    : BinArithLRpfg <0b1011, "nor"   , nor, 1>;
defm SHADD  : BinArithLRpfg <0b1100, "shadd ", shadd>;
defm SHADD2 : BinArithLRpfg <0b1101, "shadd2", shadd2>;


// Pseudo Instructions using binary arithmetic opcodes

// conditional addil/subi: add immediate only if predicate is set
let Constraints = "$rs1 = $rd", isCodeGenOnly=1, hasSideEffects = 0 in {
  def CADDi  : ALUi<0b0000, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, uimm12:$imm),
                   "add", "$rd = $rs1, $imm", []>;
  def CSUBi  : ALUi<0b0001, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, uimm12:$imm),
                   "sub", "$rd = $rs1, $imm", []>;
  def CADDl  : ALUl<0b0000, (outs RRegs:$rd), (ins predset:$p, RRegs:$rs1, i32imm:$imm),
                   "add", "$rd = $rs1, $imm", []>;
}

// move instructions
let isAsCheapAsAMove = 1, imm = 0, ImmOpNo = 0 in {
  // mov Rd = Rs ... add Rd = Rs + 0
  let hasSideEffects = 0 in
  def MOV : ALUi<0b0000, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1),
                 "mov", "$rd = $rs1", []>;

  // conditional move, like mov with setable predicate
  // $rs1 = $rd, only if the condition in $p is true, $r2 is written to $rd thus overwriting $rs1
  let Constraints = "$old = $rd", isCodeGenOnly = 1 in
  def CMOV : ALUi<0b0000, (outs RRegs:$rd), (ins predset:$p, RRegs:$old, RRegs:$new),
                  "mov", "$rd = $new",
                  [(set RRegs:$rd, (select predsel:$p, RRegs:$new, RRegs:$old))], IIC_ALUic> 
  {
    bits<5> new;
    let rs1 = new;
  }
}

// nop = sub r0 = r0 - 0
let rd = 0, rs1 = 0, imm = 0, ImmOpNo = 0, hasSideEffects = 0 in
def NOP : ALUi<0b0001, (outs ), (ins pred:$p),
               "nop", "", []>;


// neg Rd = -Rs ... sub Rd = R0 - Rs
let rs1 = 0 in
def NEG : ALUr<0b0001, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs2),
               "neg", "$rd = $rs2",
               [(set RRegs:$rd, (ineg RRegs:$rs2))]>;


// not Rd = ~Rs ... nor Rd = ~(Rs | R0)
let rs2 = 0 in
def NOT : ALUr<0b1011, (outs RRegs:$rd), (ins pred:$p, RRegs:$rs1),
               "not", "$rd = $rs1",
               [(set RRegs:$rd, (not RRegs:$rs1))]>;

// clr Rd ... add Rd = R0 + 0
let rs1 = 0, imm = 0, ImmOpNo = 0,
    hasSideEffects = 0 in
def CLR : ALUi<0b0000, (outs RRegs:$rd), (ins pred:$p),
                 "clr", "$rd", []>;



let rs1 = 0,
    ImmOpNo = 3,
    isAsCheapAsAMove = 1,
    isMoveImm = 1 in {
  // NOTE: this must be kept consistent with HasALUlVariant in PatmosInstrInfo.h

  let isReMaterializable = 1 in {
    // li Rd = Immedate ... add Rd = r0 + Immediate (short, positive)
    def LIi  : ALUi<0b0000, (outs RRegs:$rd), (ins pred:$p, uimm12:$imm),
                    "li", "$rd = $imm",
                    [(set RRegs:$rd, (add R0, uimm12:$imm))]>;
    // li Rd = Immedate ... sub Rd = r0 - Immediate (short, negative)
    def LIin : ALUi<0b0001, (outs RRegs:$rd), (ins pred:$p, nuimm12:$imm),
                    "li", "$rd = -$imm",
                    [(set RRegs:$rd, (sub R0, nuimm12:$imm))]>;
    // li Rd = Immedate ... add Rd = r0 + Immediate (long immediate)
    def LIl  : ALUl<0b0000, (outs RRegs:$rd), (ins pred:$p, i32imm:$imm),
                    "li", "$rd = $imm",
                    [(set RRegs:$rd, (add R0, (i32 imm:$imm)))]>;
  }
}


// Multiply
let Defs = [SL,SH], isCommutable = 1 in {
def MUL  : ALUm <0b0000,
              (outs ), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
              "mul", "$rs1, $rs2",
              [(PatmosMul RRegs:$rs1, RRegs:$rs2)]>;

def MULU : ALUm <0b0001,
              (outs ), (ins pred:$p, RRegs:$rs1, RRegs:$rs2),
              "mulu", "$rs1, $rs2",
              [(PatmosMulU RRegs:$rs1, RRegs:$rs2)]>;
}

// Compare

let hasSideEffects = 0 in
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

let hasSideEffects = 0 in
class CompareImm<string asmop, bits<4> opcode>
      : ALUci <opcode,
              (outs PRegs:$pd), (ins pred:$p, RRegs:$rs1, uimm5:$imm),
              asmop, "$pd = $rs1, $imm", []>;

def CMPIEQ  : CompareImm <"cmpeq" , 0b0000>;
def CMPINEQ : CompareImm <"cmpneq", 0b0001>;
def CMPILT  : CompareImm <"cmplt" , 0b0010>;
def CMPILE  : CompareImm <"cmple" , 0b0011>;
def CMPIULT : CompareImm <"cmpult", 0b0100>;
def CMPIULE : CompareImm <"cmpule", 0b0101>;
def BTESTI  : CompareImm <"btest" , 0b0110>;

// Pseudo instr ISODD uses btest opcode
let rs2 = 0 in
def ISODD : ALUc <0b0110, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs1),
                  "isodd", "$pd = $rs1",
                  [(set PRegs:$pd, (i1 (trunc RRegs:$rs1)))]>;

// Pseudo mov Pd <- Rs uses cmpneq with r0
let rs2 = 0 in
def MOVrp : ALUc <0b0001, (outs PRegs:$pd), (ins pred:$p, RRegs:$rs1),
                  "mov", "$pd = $rs1",
                  [(set PRegs:$pd, (setne RRegs:$rs1, (i32 0)))]>;



// Predicate Combine
let hasSideEffects = 0 in
class PredCombine<bits<4> opcode, string asmop, SDNode opnode>
      : ALUp <opcode,
              (outs PRegs:$pd), (ins pred:$p, predset:$ps1, predset:$ps2),
              asmop, "$pd = $ps1, $ps2", [(set PRegs:$pd, (opnode predsel:$ps1, predsel:$ps2))]>;

def POR:  PredCombine <0b0110, "por",  or>;
def PAND: PredCombine <0b0111, "pand", and>;
def PXOR: PredCombine <0b1010, "pxor", xor>;


// Pseudos:
//mov uses or opcode (or Pd = Ps | !p0)
let isAsCheapAsAMove=1, ps2 = 0b1000 in {
  let hasSideEffects = 0 in
  def PMOV: ALUp <0b0110, (outs PRegs:$pd), (ins pred:$p, predset:$ps1),
                  "pmov", "$pd = $ps1", []>;

  // conditional move, like mov with setable predicate
  // $rs1 = $rd, only if the condition in $p is true, $r2 is written to $rd thus overwriting $rs1
  let Constraints = "$old.preg = $pd", isCodeGenOnly = 1 in
  def PCMOV : ALUp<0b0110, (outs PRegs:$pd), (ins predset:$p, predset:$old, predset:$new),
                  "pmov", "$pd = $new",
                  [(set PRegs:$pd, (select predsel:$p, predsel:$new, predsel:$old))]>
  {
    bits<4> new;
    let ps1 = new;
  }


  // conditional predicate move, but the "new" operand (ps1) can have an inversion flag
  let Constraints = "$old = $pd", isCodeGenOnly = 1 in
  def PCMOV2 : ALUp<0b0110, (outs PRegs:$pd), (ins predset:$p, PRegs:$old, predset:$ps1),
                  "mov", "$pd = $ps1", []>;
}

//pnot uses pxor opcode (pxor Pd = Ps, p0)
let ps2 = 0b0000 in
def PNOT: ALUp <0b1010, (outs PRegs:$pd), (ins pred:$p, predset:$ps1),
                "pnot", "$pd = $ps1", [(set PRegs:$pd, (not predsel:$ps1))]>;


let isAsCheapAsAMove = 1,
    isMoveImm = 1,
    isReMaterializable = 1 in
{
  //set uses or opcode (or Pd = p0 | p0)
  let ps1 = 0, ps2 = 0 in
  def PSET: ALUp <0b0110, (outs PRegs:$pd), (ins pred:$p),
                  "pset", "$pd",
                  [(set PRegs:$pd, (i1 -1))]>;

  //clr uses xor opcode (xor Pd = p0 ^ p0)
  let ps1 = 0, ps2 = 0 in
  def PCLR: ALUp <0b1010, (outs PRegs:$pd), (ins pred:$p),
                  "pclr", "$pd",
                  [(set PRegs:$pd, (i1 0))]>;
}

// Bitcopy
def BCOPY : ALUb <(outs RRegs:$rd), (ins pred:$p, RRegs:$rs1, uimm5:$imm, predset:$ps),
                  "bcopy", "$rd = $rs1, $imm, $ps",
                  [(set RRegs:$rd, (or (and RRegs:$rs1, (not (shl 1, (i32 uimm5:$imm)))),
                                       (shl (i32 (zext predsel:$ps)), (i32 uimm5:$imm))))]>;

let rs1 = 0, imm = 0 in
def MOVpr : ALUb <(outs RRegs:$rd), (ins pred:$p, predset:$ps),
                  "mov", "$rd = $ps",
                  [(set RRegs:$rd, (i32 (zext predsel:$ps)))]>;
def : Pat<(anyext (i1 predsel:$p)), (MOVpr predsel:$p)>;

// move to special
let hasExtraDefRegAllocReq = 1 in
def MTS  : SPCt <(outs SRegs:$sd), (ins pred:$p, RRegs:$rs), "mts", "$sd = $rs", [] >;

// move from special
let hasExtraSrcRegAllocReq = 1, hasSideEffects = 0 in
def MFS  : SPCf <(outs RRegs:$rd), (ins pred:$p, SRegs:$ss), "mfs", "$rd = $ss", [] >;



// Load typed

let hasSideEffects = 0 in // mayLoad is set in format
multiclass LoadTyped<bits<3> ty, Operand immTy, string asmop>
{
  // load from stack cache
  let Uses = [ST] in
  def S  : LDT<ty, 0b0, 0b0, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               asmop # "s", "$rd = [$ra$imm]",
               [], IIC_LDs>;

  // load from local memory
  def L  : LDT<ty, 0b0, 0b1, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               asmop # "l", "$rd = [$ra$imm]",
               [], IIC_LD>;

  // load from data cache
  let mayStall = 1 in
  def C  : LDT<ty, 0b1, 0b0, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               asmop # "c", "$rd = [$ra$imm]",
               [], IIC_LD>;

  // load from main memory
  let mayStall = 1 in
  def M  : LDT<ty, 0b1, 0b1, (outs RRegs:$rd),
               (ins pred:$p, RRegs:$ra, immTy:$imm),
               asmop # "m", "$rd = [$ra$imm]",
               [], IIC_LD>;
}

let ImmShift = 2, ImmSigned = 0 in defm LW  : LoadTyped<0b000, uimm7s2, "lw" >;
let ImmShift = 1, ImmSigned = 0 in defm LH  : LoadTyped<0b001, uimm7s1, "lh" >;
let ImmShift = 0, ImmSigned = 0 in defm LB  : LoadTyped<0b010, uimm7  , "lb" >;
let ImmShift = 1, ImmSigned = 0 in defm LHU : LoadTyped<0b011, uimm7s1, "lhu">;
let ImmShift = 0, ImmSigned = 0 in defm LBU : LoadTyped<0b100, uimm7  , "lbu">;



// Store typed

let hasSideEffects = 0 in // mayStore is set in format
multiclass StoreTyped<bits<3> ty, Operand immTy, string asmop>
{
  // store to stack cache
  let Uses = [ST] in
  def S  : STT<ty, 0b00, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               asmop # "s", "[$ra$imm] = $rs",
               [], IIC_STs>;

  // store to local memory
  def L  : STT<ty, 0b01, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               asmop # "l", "[$ra$imm] = $rs",
               [], IIC_ST>;

  // store to data cache (and to main memory; with data cache allocation?)
  let mayStall = 1 in
  def C  : STT<ty, 0b10, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               asmop # "c", "[$ra$imm] = $rs",
               [], IIC_ST>;

  // store to main memory (no data cache allocate)
  let mayStall = 1 in
  def M  : STT<ty, 0b11, (outs /*empty*/),
               (ins pred:$p, RRegs:$ra, immTy:$imm, RRegs:$rs),
               asmop # "m", "[$ra$imm] = $rs",
               [], IIC_ST>;
}


let ImmShift = 2, ImmSigned = 0 in defm SW  : StoreTyped<0b000, uimm7s2, "sw">;
let ImmShift = 1, ImmSigned = 0 in defm SH  : StoreTyped<0b001, uimm7s1, "sh">;
let ImmShift = 0, ImmSigned = 0 in defm SB  : StoreTyped<0b010, uimm7  , "sb">;


//===----------------------------------------------------------------------===//
// Non-Instruction Patterns
//===----------------------------------------------------------------------===//


// Patterns are in a separate file
include "PatmosInstrPatterns.td"


