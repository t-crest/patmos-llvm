//===- PatmosInstrPatterns.td - Patmos Instruction Formats----*- tblgen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes Instruction selection patterns for Patmos.
//
//===----------------------------------------------------------------------===//

// zero in R0
def : Pat<(i32 0), (i32 R0)>;
// true in P0
def : Pat<(i1 -1), (i1  P0)>;

// load immediate
def : Pat<(i32 uimm12:$imm),  (LIi uimm12:$imm)>;
def : Pat<(i32 nuimm12:$imm), (LIin nuimm12:$imm)>;
def : Pat<(i32 imm:$imm),     (LIl (i32 imm:$imm))>;

// for zext-ing an i1 to i32: conditionally load 1 into an RReg
def : Pat<(zext (i1 predsel:$p)), (CADDi predsel:$p, (CLR), (i32 1))>;
def : Pat<(sext (i1 predsel:$p)), (CADDi predsel:$p, (CLR), (i32 1))>;
def : Pat<(anyext (i1 predsel:$p)), (CADDi predsel:$p, (CLR), (i32 1))>;

def : Pat<(sext_inreg RRegs:$r, i1), (SRAi (SLi RRegs:$r, (i32 31)), (i32 31))>;

//def : Pat<(select RRegs:$rp, RRegs:$rs1, RRegs:$rs2), (CMOV (BTEST RRegs:$rp, 0), RRegs:$rs1, RRegs:$rs2)>;

def : Pat<(select predsel:$p, RRegs:$new, uimm12:$imm), (CMOV predsel:$p, (LIi uimm12:$imm), RRegs:$new)>;
def : Pat<(select predsel:$p, RRegs:$new, nuimm12:$imm), (CMOV predsel:$p, (LIin nuimm12:$imm), RRegs:$new)>;
def : Pat<(select predsel:$p, RRegs:$new, (i32 imm:$imm)), (CMOV predsel:$p, (LIl (i32 imm:$imm)), RRegs:$new)>;

// Arithmetic optimizations
def : Pat<(add RRegs:$rs, nuimm12:$imm), (SUBi RRegs:$rs, nuimm12:$imm)>;



// // jump-table.
def : Pat<(i32 tjumptable:$dst), (LIl tjumptable:$dst)>;
def : Pat<(i32 jtpat:$dst), (LIl jtpat:$dst)>;

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

def : Pat<(PatmosCall tglobaladdr:$sym), (CALL tglobaladdr:$sym)>;
def : Pat<(PatmosCall texternalsym:$sym), (CALL texternalsym:$sym)>;

def : Pat<(PatmosReturn ), (RET RFB, RFO)>;

// NOTE: following works when not expanding MUL to S/UMUL_LOHI
// multiplication (requiring only low part)
//def : Pat<(mul RRegs:$rs1, RRegs:$rs2),
//          (COPY_TO_REGCLASS (MUL RRegs:$rs1, RRegs:$rs2), RRegs)>;


// Compare patterns

def : Pat<(seteq  RRegs:$a, RRegs:$b), (CMPEQ  RRegs:$a, RRegs:$b)>;
def : Pat<(setne  RRegs:$a, RRegs:$b), (CMPNEQ RRegs:$a, RRegs:$b)>;
def : Pat<(setlt  RRegs:$a, RRegs:$b), (CMPLT  RRegs:$a, RRegs:$b)>;
def : Pat<(setle  RRegs:$a, RRegs:$b), (CMPLE  RRegs:$a, RRegs:$b)>;
def : Pat<(setult RRegs:$a, RRegs:$b), (CMPULT RRegs:$a, RRegs:$b)>;
def : Pat<(setule RRegs:$a, RRegs:$b), (CMPULE RRegs:$a, RRegs:$b)>;
// we find paterns for inverted conditions (instead of Patmos pseudo instructions)
def : Pat<(setgt  RRegs:$b, RRegs:$a), (CMPLT  RRegs:$a, RRegs:$b)>;
def : Pat<(setge  RRegs:$b, RRegs:$a), (CMPLE  RRegs:$a, RRegs:$b)>;
def : Pat<(setugt RRegs:$b, RRegs:$a), (CMPULT RRegs:$a, RRegs:$b)>;
def : Pat<(setuge RRegs:$b, RRegs:$a), (CMPULE RRegs:$a, RRegs:$b)>;

def : Pat<(setne (and RRegs:$a, (shl (i32 1), RRegs:$b)), (i32 0)),
          (BTEST RRegs:$a, RRegs:$b)>;
def : Pat<(setne (and RRegs:$a, uimm5mask:$b), (i32 0)),
          (BTEST RRegs:$a, (LIi uimm5mask:$b))>;
def : Pat<(seteq (and RRegs:$a, uimm5mask:$b), uimm5mask:$b),
          (BTEST RRegs:$a, (LIi uimm5mask:$b))>;

// Predicate Combine patterns

def : Pat<(or  predsel:$ps1, predsel:$ps2), (POR  predsel:$ps1, predsel:$ps2)>;
def : Pat<(and predsel:$ps1, predsel:$ps2), (PAND predsel:$ps1, predsel:$ps2)>;
def : Pat<(xor predsel:$ps1, predsel:$ps2), (PXOR predsel:$ps1, predsel:$ps2)>;
def : Pat<(notcc (or predsel:$ps1, predsel:$ps2)),
                                            (PNOR predsel:$ps1, predsel:$ps2)>;


def : Pat<(setne  predsel:$a, (i1 -1)), (PNOT predsel:$a)>;

def : Pat<(i1 (trunc RRegs:$a)), (ISODD RRegs:$a)>;


// inst_C: instruction to use for data cache load
// inst_M: instruction to use for uncached load
multiclass LoadTypedPatterns<PatmosInst inst_C, PatFrag immFg, PatFrag pfg>
{
  // load register indirect from cache
  def ra : Pat<(pfg RRegs:$ra), (inst_C RRegs:$ra, 0)>;

  // load register indirect + immediate from cache
  def raimm : Pat<(pfg (add RRegs:$ra, immFg:$imm)),
                  (inst_C RRegs:$ra, immFg:$imm)>;

  // load register indirect from cache
  def fi : Pat<(pfg fipat:$fi), (inst_C fipat:$fi, 0)>;

  // load register indirect from cache
  def fiimm : Pat<(pfg (add fipat:$fi, immFg:$imm)),
                  (inst_C fipat:$fi, immFg:$imm)>;

  def fior : Pat<(pfg (or fipat:$fi, immFg:$imm)),
                  (inst_C fipat:$fi, immFg:$imm)>;

  // TODO: more patterns here
}

class cachedLoad<PatFrag patLoad> : PatFrag<(ops node:$ptr), (patLoad node:$ptr), [{
		return !cast<MemSDNode>(N)->isVolatile() && !cast<MemSDNode>(N)->isNonTemporal();
	    }]>;

class uncachedLoad<PatFrag patLoad> : PatFrag<(ops node:$ptr), (patLoad node:$ptr), [{
		return cast<MemSDNode>(N)->isVolatile() || cast<MemSDNode>(N)->isNonTemporal();
	    }]>;

// feel free to use multiclass if you find out how to make them work ..
defm LW   : LoadTypedPatterns<LWC , imm7s2, cachedLoad<load>>;
defm LH   : LoadTypedPatterns<LHC , imm7s1, cachedLoad<sextloadi16>>;
defm LB   : LoadTypedPatterns<LBC , imm7  , cachedLoad<sextloadi8>>;
defm LHU  : LoadTypedPatterns<LHUC, imm7s1, cachedLoad<zextloadi16>>;
defm LBU  : LoadTypedPatterns<LBUC, imm7  , cachedLoad<zextloadi8>>;
defm LHE  : LoadTypedPatterns<LHUC, imm7s1, cachedLoad<extloadi16>>;
defm LBE  : LoadTypedPatterns<LBUC, imm7  , cachedLoad<extloadi8>>;
defm LWM  : LoadTypedPatterns<LWM , imm7s2, uncachedLoad<load>>;
defm LHM  : LoadTypedPatterns<LHM , imm7s1, uncachedLoad<sextloadi16>>;
defm LBM  : LoadTypedPatterns<LBM , imm7  , uncachedLoad<sextloadi8>>;
defm LHUM : LoadTypedPatterns<LHUM, imm7s1, uncachedLoad<zextloadi16>>;
defm LBUM : LoadTypedPatterns<LBUM, imm7  , uncachedLoad<zextloadi8>>;
defm LHEM : LoadTypedPatterns<LHUM, imm7s1, uncachedLoad<extloadi16>>;
defm LBEM : LoadTypedPatterns<LBUM, imm7  , uncachedLoad<extloadi8>>;


// store patterns


multiclass StoreTypedPatterns<PatmosInst inst_C, PatFrag immFg, PatFrag pfg>
{
  // load register indirect from cache
  def ra : Pat<(pfg RRegs:$rs, RRegs:$ra),
               (inst_C RRegs:$ra, 0, RRegs:$rs)>;

  // load register indirect + immediate from cache
  def raimm : Pat<(pfg RRegs:$rs, (add RRegs:$ra, immFg:$imm)),
                  (inst_C RRegs:$ra, immFg:$imm, RRegs:$rs)>;

  // load register indirect from cache
  def fi : Pat<(pfg RRegs:$rs, fipat:$fi),
               (inst_C fipat:$fi, 0, RRegs:$rs)>;

  // load register indirect from cache
  def fiimm : Pat<(pfg RRegs:$rs, (add fipat:$fi, immFg:$imm)),
                  (inst_C fipat:$fi, immFg:$imm, RRegs:$rs)>;

  def fior  : Pat<(pfg RRegs:$rs, (or fipat:$fi, immFg:$imm)),
                  (inst_C fipat:$fi, immFg:$imm, RRegs:$rs)>;

  // TODO: more patterns here
}

class cachedStore<PatFrag patStore> : PatFrag<(ops node:$val, node:$ptr), (patStore node:$val, node:$ptr), [{
		return !cast<MemSDNode>(N)->isVolatile() && !cast<MemSDNode>(N)->isNonTemporal();
	    }]>;

class uncachedStore<PatFrag patStore> : PatFrag<(ops node:$val, node:$ptr), (patStore node:$val, node:$ptr), [{
		return cast<MemSDNode>(N)->isVolatile() || cast<MemSDNode>(N)->isNonTemporal();
	    }]>;

defm SW  : StoreTypedPatterns<SWC, imm7s2, cachedStore<store>>;
defm SH  : StoreTypedPatterns<SHC, imm7s1, cachedStore<truncstorei16>>;
defm SB  : StoreTypedPatterns<SBC, imm7  , cachedStore<truncstorei8>>;
defm SWM : StoreTypedPatterns<SWM, imm7s2, uncachedStore<store>>;
defm SHM : StoreTypedPatterns<SHM, imm7s1, uncachedStore<truncstorei16>>;
defm SBM : StoreTypedPatterns<SBM, imm7  , uncachedStore<truncstorei8>>;

