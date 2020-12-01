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




// sign extensions of single bit
def : Pat<(sext (i1 predsel:$p)), (CSUBi predsel:$p, (CLR), (i32 1))>;
def : Pat<(sext_inreg RRegs:$r, i1), (SRAi (SLi RRegs:$r, (i32 31)), (i32 31))>;


// i1 promotion workarounds
def : Pat<(add predsel:$p1, predsel:$p2), (PXOR predsel:$p1, predsel:$p2)>;
def : Pat<(sub predsel:$p1, predsel:$p2), (PXOR predsel:$p1, predsel:$p2)>;

// SET_CC operations, for combining results of SetCC.
// TODO extend this for other operators, if needed.
// TODO there should be a better way to do this ?!
def : Pat<(setcc (i1 predsel:$p1), (i1 predsel:$p2), SETNE), (PXOR predsel:$p1, predsel:$p2)>;


//def : Pat<(select RRegs:$rp, RRegs:$rs1, RRegs:$rs2), (CMOV (BTEST RRegs:$rp, 0), RRegs:$rs1, RRegs:$rs2)>;


def : Pat<(select predsel:$p, RRegs:$new, uimm12:$imm), (CMOV predsel:$p, (LIi uimm12:$imm), RRegs:$new)>;
def : Pat<(select predsel:$p, RRegs:$new, nuimm12:$imm), (CMOV predsel:$p, (LIin nuimm12:$imm), RRegs:$new)>;
def : Pat<(select predsel:$p, RRegs:$new, (i32 imm:$imm)), (CMOV predsel:$p, (LIl (i32 imm:$imm)), RRegs:$new)>;


// Conditional overwriting patterns //////////////////////////////////////////

/*
These patterns are optimizations with predicated instructions
by generalizing conditional move, such that they conditionally overwrite
a value computed before:

    rd := ... old ...
(p) rd := ... new op...

achieved by  rd := sel (p), (new op), old
with constraint rd = old.

We have to treat old=0 as special case, because this will match R0
and regalloc will fail upon the constraint.
*/

// short immediate
multiclass pat_i_ow<string I, SDPatternOperator op> {
  // special case 0
  def : Pat<(select predsel:$p, (op RRegs:$rs1, (i32 uimm12:$imm)), 0),
            (!cast<PatmosInst>(I#i_ow) predsel:$p, RRegs:$rs1, (i32 uimm12:$imm), (CLR))>;
  def : Pat<(select predselinv:$p, 0, (op RRegs:$rs1, (i32 uimm12:$imm))),
            (!cast<PatmosInst>(I#i_ow) predselinv:$p, RRegs:$rs1, (i32 uimm12:$imm), (CLR))>;
  // usual case
  def : Pat<(select predsel:$p, (op RRegs:$rs1, (i32 uimm12:$imm)), RRegs:$old),
            (!cast<PatmosInst>(I#i_ow) predsel:$p, RRegs:$rs1, (i32 uimm12:$imm), RRegs:$old)>;
  def : Pat<(select predselinv:$p, RRegs:$old, (op RRegs:$rs1, (i32 uimm12:$imm))),
            (!cast<PatmosInst>(I#i_ow) predselinv:$p, RRegs:$rs1, (i32 uimm12:$imm), RRegs:$old)>;
}

// long immediate
multiclass pat_l_ow<string I, SDPatternOperator op> {
  // special case 0
  def : Pat<(select predsel:$p, (op RRegs:$rs1, (i32 imm:$imm)), 0),
            (!cast<PatmosInst>(I#l_ow) predsel:$p, RRegs:$rs1, (i32 imm:$imm), (CLR))>;
  def : Pat<(select predselinv:$p, 0, (op RRegs:$rs1, (i32 imm:$imm))),
            (!cast<PatmosInst>(I#l_ow) predselinv:$p, RRegs:$rs1, (i32 imm:$imm), (CLR))>;
  // usual case
  def : Pat<(select predsel:$p, (op RRegs:$rs1, (i32 imm:$imm)), RRegs:$old),
            (!cast<PatmosInst>(I#l_ow) predsel:$p, RRegs:$rs1, (i32 imm:$imm), RRegs:$old)>;
  def : Pat<(select predselinv:$p, RRegs:$old, (op RRegs:$rs1, (i32 imm:$imm))),
            (!cast<PatmosInst>(I#l_ow) predselinv:$p, RRegs:$rs1, (i32 imm:$imm), RRegs:$old)>;
}

// register
multiclass pat_r_ow<string I, SDPatternOperator op> {
  // special case 0
  def : Pat<(select predsel:$p, (op RRegs:$rs1, RRegs:$rs2), 0),
            (!cast<PatmosInst>(I#r_ow) predsel:$p, RRegs:$rs1, RRegs:$rs2, (CLR))>;
  def : Pat<(select predselinv:$p, 0, (op RRegs:$rs1, RRegs:$rs2)),
            (!cast<PatmosInst>(I#r_ow) predselinv:$p, RRegs:$rs1, RRegs:$rs2, (CLR))>;
  // usual case
  def : Pat<(select predsel:$p, (op RRegs:$rs1, RRegs:$rs2), RRegs:$old),
            (!cast<PatmosInst>(I#r_ow) predsel:$p, RRegs:$rs1, RRegs:$rs2, RRegs:$old)>;
  def : Pat<(select predselinv:$p, RRegs:$old, (op RRegs:$rs1, RRegs:$rs2)),
            (!cast<PatmosInst>(I#r_ow) predselinv:$p, RRegs:$rs1, RRegs:$rs2, RRegs:$old)>;
}


multiclass pat_ilr_ow<string I, SDPatternOperator op> {
  defm  : pat_i_ow<I, op>;
  defm  : pat_l_ow<I, op>;
  defm  : pat_r_ow<I, op>;
}

multiclass pat_lr_ow<string I, SDPatternOperator op> {
  defm  : pat_l_ow<I, op>;
  defm  : pat_r_ow<I, op>;
}

multiclass pat_ir_ow<string I, SDPatternOperator op> {
  defm  : pat_i_ow<I, op>;
  defm  : pat_r_ow<I, op>;
}

// with short immediates:
defm : pat_ilr_ow<"ADD", add>;
defm : pat_ilr_ow<"SUB", sub>;
defm : pat_ilr_ow<"XOR", xor>;
defm : pat_ir_ow <"SL" , shl>;
defm : pat_ir_ow <"SR" , srl>;
defm : pat_ir_ow <"SRA", sra>;
defm : pat_ilr_ow<"AND", and>;
// no short immediates:
defm : pat_lr_ow <"NOR"   , nor   >;
defm : pat_lr_ow <"SHADD" , shadd >;
defm : pat_lr_ow <"SHADD2", shadd2>;


// Arithmetic optimizations //////////////////////////////////////////////////

def : Pat<(add RRegs:$rs, nuimm12:$imm), (SUBi RRegs:$rs, nuimm12:$imm)>;

// p={0|1} * sth = (p) LI/MOV sth
def : Pat<(mul (zext (i1 predsel:$p)), uimm12:$imm),
          (CADDi predsel:$p, (CLR), uimm12:$imm)>;
def : Pat<(mul (zext (i1 predsel:$p)), nuimm12:$imm),
          (CSUBi predsel:$p, (CLR), nuimm12:$imm)>;
def : Pat<(mul (zext (i1 predsel:$p)), (i32 imm:$imm)),
          (CADDl predsel:$p, (CLR), (i32 imm:$imm))>;
def : Pat<(mul (zext (i1 predsel:$p)), RRegs:$rs),
          (CMOV predsel:$p, (CLR), RRegs:$rs)>;


// TODO nor patterns, with immediates

// shadd/shadd2 instead of mul 3/5
def : Pat<(mul RRegs:$r, (i32 3)), (SHADDr RRegs:$r, RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32 5)), (SHADD2r RRegs:$r, RRegs:$r)>;

// mul x, (3 * 2^n)
def : Pat<(mul RRegs:$r, (i32   6)), (SLi (SHADDr RRegs:$r, RRegs:$r), 1)>;
def : Pat<(mul RRegs:$r, (i32  12)), (SLi (SHADDr RRegs:$r, RRegs:$r), 2)>;
def : Pat<(mul RRegs:$r, (i32  24)), (SLi (SHADDr RRegs:$r, RRegs:$r), 3)>;
def : Pat<(mul RRegs:$r, (i32  48)), (SLi (SHADDr RRegs:$r, RRegs:$r), 4)>;
def : Pat<(mul RRegs:$r, (i32  96)), (SLi (SHADDr RRegs:$r, RRegs:$r), 5)>;
def : Pat<(mul RRegs:$r, (i32 192)), (SLi (SHADDr RRegs:$r, RRegs:$r), 6)>;
def : Pat<(mul RRegs:$r, (i32 384)), (SLi (SHADDr RRegs:$r, RRegs:$r), 7)>;
def : Pat<(mul RRegs:$r, (i32 768)), (SLi (SHADDr RRegs:$r, RRegs:$r), 8)>;
// mul x, (5 * 2^n)
def : Pat<(mul RRegs:$r, (i32   10)), (SLi (SHADD2r RRegs:$r, RRegs:$r), 1)>;
def : Pat<(mul RRegs:$r, (i32   20)), (SLi (SHADD2r RRegs:$r, RRegs:$r), 2)>;
def : Pat<(mul RRegs:$r, (i32   40)), (SLi (SHADD2r RRegs:$r, RRegs:$r), 3)>;
def : Pat<(mul RRegs:$r, (i32   80)), (SLi (SHADD2r RRegs:$r, RRegs:$r), 4)>;
def : Pat<(mul RRegs:$r, (i32  160)), (SLi (SHADD2r RRegs:$r, RRegs:$r), 5)>;
def : Pat<(mul RRegs:$r, (i32  320)), (SLi (SHADD2r RRegs:$r, RRegs:$r), 6)>;
def : Pat<(mul RRegs:$r, (i32  640)), (SLi (SHADD2r RRegs:$r, RRegs:$r), 7)>;
def : Pat<(mul RRegs:$r, (i32 1280)), (SLi (SHADD2r RRegs:$r, RRegs:$r), 8)>;
// mul x, (2^n + 1) = x<<n + x
// for n>2 because n<=2 is covered by the above patterns
def : Pat<(mul RRegs:$r, (i32   9)), (ADDr (SLi RRegs:$r, 3), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32  17)), (ADDr (SLi RRegs:$r, 4), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32  33)), (ADDr (SLi RRegs:$r, 5), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32  65)), (ADDr (SLi RRegs:$r, 6), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32 129)), (ADDr (SLi RRegs:$r, 7), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32 257)), (ADDr (SLi RRegs:$r, 8), RRegs:$r)>;
// mul x, (2^n - 1) = x<<n - x
// for n>2 because n<=2 is covered by the above patterns
def : Pat<(mul RRegs:$r, (i32   7)), (SUBr (SLi RRegs:$r, 3), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32  15)), (SUBr (SLi RRegs:$r, 4), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32  31)), (SUBr (SLi RRegs:$r, 5), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32  63)), (SUBr (SLi RRegs:$r, 6), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32 127)), (SUBr (SLi RRegs:$r, 7), RRegs:$r)>;
def : Pat<(mul RRegs:$r, (i32 255)), (SUBr (SLi RRegs:$r, 8), RRegs:$r)>;



// jump-table.
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

def : Pat<(PatmosCall tglobaladdr:$sym), (CALL tglobaladdr:$sym)>, Requires<[NotLargeCode]>;
def : Pat<(PatmosCall texternalsym:$sym), (CALL texternalsym:$sym)>, Requires<[NotLargeCode]>;
def : Pat<(PatmosCall tglobaladdr:$sym), (CALLR (LIl tglobaladdr:$sym))>;
def : Pat<(PatmosCall texternalsym:$sym), (CALLR (LIl texternalsym:$sym))>;

def : Pat<(PatmosReturn ), (RET)>;

// inverted branch condition
def : Pat<(brcond (notcc predselinv:$p), bb:$target), (BR predselinv:$p, bb:$target)>;


// multiplication (requiring only low part)
def : Pat<(mul RRegs:$rs1, RRegs:$rs2),
          (COPY_TO_REGCLASS (MUL RRegs:$rs1, RRegs:$rs2), RRegs)>;


// Compare patterns

def : Pat<(seteq  RRegs:$a, RRegs:$b), (CMPEQ  RRegs:$a, RRegs:$b)>;
def : Pat<(setne  RRegs:$a, RRegs:$b), (CMPNEQ RRegs:$a, RRegs:$b)>;
def : Pat<(setlt  RRegs:$a, RRegs:$b), (CMPLT  RRegs:$a, RRegs:$b)>;
def : Pat<(setle  RRegs:$a, RRegs:$b), (CMPLE  RRegs:$a, RRegs:$b)>;
def : Pat<(setult RRegs:$a, RRegs:$b), (CMPULT RRegs:$a, RRegs:$b)>;
def : Pat<(setule RRegs:$a, RRegs:$b), (CMPULE RRegs:$a, RRegs:$b)>;
// we find patterns for inverted conditions (instead of Patmos pseudo instructions)
def : Pat<(setgt  RRegs:$b, RRegs:$a), (CMPLT  RRegs:$a, RRegs:$b)>;
def : Pat<(setge  RRegs:$b, RRegs:$a), (CMPLE  RRegs:$a, RRegs:$b)>;
def : Pat<(setugt RRegs:$b, RRegs:$a), (CMPULT RRegs:$a, RRegs:$b)>;
def : Pat<(setuge RRegs:$b, RRegs:$a), (CMPULE RRegs:$a, RRegs:$b)>;

def : Pat<(seteq  RRegs:$a, uimm5:$b), (CMPIEQ  RRegs:$a, uimm5:$b)>;
def : Pat<(setne  RRegs:$a, uimm5:$b), (CMPINEQ RRegs:$a, uimm5:$b)>;
def : Pat<(setlt  RRegs:$a, uimm5:$b), (CMPILT  RRegs:$a, uimm5:$b)>;
def : Pat<(setle  RRegs:$a, uimm5:$b), (CMPILE  RRegs:$a, uimm5:$b)>;
def : Pat<(setult RRegs:$a, uimm5:$b), (CMPIULT RRegs:$a, uimm5:$b)>;
def : Pat<(setule RRegs:$a, uimm5:$b), (CMPIULE RRegs:$a, uimm5:$b)>;

// FIXME DP:
// we would also need a pattern for == 0, and invert the use of the result
// but I have no clue how to achieve that in an elegant way.
def : Pat<(setne (and RRegs:$a, (shl (i32 1), RRegs:$b)), (i32 0)),
          (BTEST RRegs:$a, RRegs:$b)>;
def : Pat<(setne (and RRegs:$a, uimm5mask:$b), (i32 0)),
          (BTESTI RRegs:$a, uimm5mask:$b)>;
def : Pat<(seteq (and RRegs:$a, uimm5imask:$b), uimm5imask:$b),
          (BTESTI RRegs:$a, uimm5imask:$b)>;

// Predicate Combine patterns

def : Pat<(or  predsel:$ps1, predsel:$ps2), (POR  predsel:$ps1, predsel:$ps2)>;
def : Pat<(and predsel:$ps1, predsel:$ps2), (PAND predsel:$ps1, predsel:$ps2)>;
def : Pat<(xor predsel:$ps1, predsel:$ps2), (PXOR predsel:$ps1, predsel:$ps2)>;

// negated operands
def : Pat<(or  (not predselinv:$ps1), predsel:$ps2), (POR predselinv:$ps1, predsel:$ps2)>;
def : Pat<(or  predsel:$ps1, (not predselinv:$ps2)), (POR predsel:$ps1, predselinv:$ps2)>;
def : Pat<(or  (not predselinv:$ps1), (not predselinv:$ps2)), (POR predselinv:$ps1, predselinv:$ps2)>;
def : Pat<(and (not predselinv:$ps1), predsel:$ps2), (PAND predselinv:$ps1, predsel:$ps2)>;
def : Pat<(and predsel:$ps1, (not predselinv:$ps2)), (PAND predsel:$ps1, predselinv:$ps2)>;
def : Pat<(and (not predselinv:$ps1), (not predselinv:$ps2)), (PAND predselinv:$ps1, predselinv:$ps2)>;
def : Pat<(xor (not predselinv:$ps1), predsel:$ps2), (PXOR predselinv:$ps1, predsel:$ps2)>;
def : Pat<(xor predsel:$ps1, (not predselinv:$ps2)), (PXOR predsel:$ps1, predselinv:$ps2)>;
def : Pat<(xor (not predselinv:$ps1), (not predselinv:$ps2)), (PXOR predselinv:$ps1, predselinv:$ps2)>;

// laws of DeMorgan
def : Pat<(notcc (or  predselinv:$ps1, predselinv:$ps2)), (PAND predselinv:$ps1, predselinv:$ps2)>;
def : Pat<(notcc (and predselinv:$ps1, predselinv:$ps2)), (POR  predselinv:$ps1, predselinv:$ps2)>;

// invert condition
def : Pat<(notcc predsel:$ps1), (PNOT predsel:$ps1)>;




// inst_C: instruction to use for data load
multiclass LoadTypedPatterns<PatmosInst inst, PatFrag immFg, PatFrag immBaseFg, PatFrag pfg>
{
  // load register indirect from cache
  def ra : Pat<(pfg RRegs:$ra), (inst RRegs:$ra, 0)>;

  // load register indirect + immediate from cache
  def raimm : Pat<(pfg (add RRegs:$ra, immFg:$imm)),
                  (inst RRegs:$ra, immFg:$imm)>;

  // load register indirect from cache
  def fi : Pat<(pfg fipat:$fi), (inst fipat:$fi, 0)>;

  // load register indirect from cache
  def fiimm : Pat<(pfg (add fipat:$fi, immFg:$imm)),
                  (inst fipat:$fi, immFg:$imm)>;

  def fior : Pat<(pfg (or fipat:$fi, immFg:$imm)),
                  (inst fipat:$fi, immFg:$imm)>;

  // load with short immediate address from cache
  def imm : Pat<(pfg immFg:$imm), (inst R0, immFg:$imm)>;

  // load with medium immediate address from cache
  def mimm : Pat<(pfg uimm12:$imm), (inst (LIi immBaseFg:$imm), immFg:$imm)>;

  // load with long immediate address from cache
  def limm : Pat<(pfg imm:$imm), (inst (LIl immBaseFg:$imm), immFg:$imm)>;

  // TODO: more patterns here
}

class cachedLoad<PatFrag patLoad> : PatFrag<(ops node:$ptr), (patLoad node:$ptr), [{
		return cast<MemSDNode>(N)->getAddressSpace() == 0 && !cast<MemSDNode>(N)->isNonTemporal();
	    }]>;

class uncachedLoad<PatFrag patLoad> : PatFrag<(ops node:$ptr), (patLoad node:$ptr), [{
		return (cast<MemSDNode>(N)->getAddressSpace() == 0 && cast<MemSDNode>(N)->isNonTemporal()) ||
		        cast<MemSDNode>(N)->getAddressSpace() == 3;
	    }]>;

class spmLoad<PatFrag patLoad> : PatFrag<(ops node:$ptr), (patLoad node:$ptr), [{
		return cast<MemSDNode>(N)->getAddressSpace() == 1;
	    }]>;

// feel free to use multiclass if you find out how to make them work ..
defm LW   : LoadTypedPatterns<LWC , uimm7s2, immBase7s2, cachedLoad<load>>;
defm LH   : LoadTypedPatterns<LHC , uimm7s1, immBase7s1, cachedLoad<sextloadi16>>;
defm LB   : LoadTypedPatterns<LBC , uimm7  , immBase7  , cachedLoad<sextloadi8>>;
defm LHU  : LoadTypedPatterns<LHUC, uimm7s1, immBase7s1, cachedLoad<zextloadi16>>;
defm LBU  : LoadTypedPatterns<LBUC, uimm7  , immBase7  , cachedLoad<zextloadi8>>;
defm LHE  : LoadTypedPatterns<LHUC, uimm7s1, immBase7s1, cachedLoad<extloadi16>>;
defm LBE  : LoadTypedPatterns<LBUC, uimm7  , immBase7  , cachedLoad<extloadi8>>;
defm LWM  : LoadTypedPatterns<LWM , uimm7s2, immBase7s2, uncachedLoad<load>>;
defm LHM  : LoadTypedPatterns<LHM , uimm7s1, immBase7s1, uncachedLoad<sextloadi16>>;
defm LBM  : LoadTypedPatterns<LBM , uimm7  , immBase7  , uncachedLoad<sextloadi8>>;
defm LHUM : LoadTypedPatterns<LHUM, uimm7s1, immBase7s1, uncachedLoad<zextloadi16>>;
defm LBUM : LoadTypedPatterns<LBUM, uimm7  , immBase7  , uncachedLoad<zextloadi8>>;
defm LHEM : LoadTypedPatterns<LHUM, uimm7s1, immBase7s1, uncachedLoad<extloadi16>>;
defm LBEM : LoadTypedPatterns<LBUM, uimm7  , immBase7  , uncachedLoad<extloadi8>>;

defm LWL  : LoadTypedPatterns<LWL , uimm7s2, immBase7s2, spmLoad<load>>;
defm LHL  : LoadTypedPatterns<LHL , uimm7s1, immBase7s1, spmLoad<sextloadi16>>;
defm LBL  : LoadTypedPatterns<LBL , uimm7  , immBase7  , spmLoad<sextloadi8>>;
defm LHUL : LoadTypedPatterns<LHUL, uimm7s1, immBase7s1, spmLoad<zextloadi16>>;
defm LBUL : LoadTypedPatterns<LBUL, uimm7  , immBase7  , spmLoad<zextloadi8>>;
defm LHEL : LoadTypedPatterns<LHUL, uimm7s1, immBase7s1, spmLoad<extloadi16>>;
defm LBEL : LoadTypedPatterns<LBUL, uimm7  , immBase7  , spmLoad<extloadi8>>;

// store patterns


multiclass StoreTypedPatterns<PatmosInst inst, PatFrag immFg, PatFrag immBaseFg, PatFrag pfg>
{
  // store register indirect to cache
  def ra : Pat<(pfg RRegs:$rs, RRegs:$ra),
               (inst RRegs:$ra, 0, RRegs:$rs)>;

  // store register indirect + immediate to cache
  def raimm : Pat<(pfg RRegs:$rs, (add RRegs:$ra, immFg:$imm)),
                  (inst RRegs:$ra, immFg:$imm, RRegs:$rs)>;

  // store register indirect to cache
  def fi : Pat<(pfg RRegs:$rs, fipat:$fi),
               (inst fipat:$fi, 0, RRegs:$rs)>;

  // store register indirect to cache
  def fiimm : Pat<(pfg RRegs:$rs, (add fipat:$fi, immFg:$imm)),
                  (inst fipat:$fi, immFg:$imm, RRegs:$rs)>;

  def fior  : Pat<(pfg RRegs:$rs, (or fipat:$fi, immFg:$imm)),
                  (inst fipat:$fi, immFg:$imm, RRegs:$rs)>;

  // store with short immediate address to cache
  def imm : Pat<(pfg RRegs:$rs, immFg:$imm),
                (inst R0, immFg:$imm, RRegs:$rs)>;

  // store with medium immediate address to cache
  def mimm : Pat<(pfg RRegs:$rs, uimm12:$imm),
                 (inst (LIi immBaseFg:$imm), immFg:$imm, RRegs:$rs)>;

  // store with long immediate address to cache
  def limm : Pat<(pfg RRegs:$rs, imm:$imm),
                 (inst (LIl immBaseFg:$imm), immFg:$imm, RRegs:$rs)>;

  // TODO: more patterns here
}

class cachedStore<PatFrag patStore> : PatFrag<(ops node:$val, node:$ptr), (patStore node:$val, node:$ptr), [{
		return cast<MemSDNode>(N)->getAddressSpace() == 0 && !cast<MemSDNode>(N)->isNonTemporal();
	    }]>;

class uncachedStore<PatFrag patStore> : PatFrag<(ops node:$val, node:$ptr), (patStore node:$val, node:$ptr), [{
		return (cast<MemSDNode>(N)->getAddressSpace() == 0 && cast<MemSDNode>(N)->isNonTemporal()) ||
		        cast<MemSDNode>(N)->getAddressSpace() == 3;
	    }]>;

class spmStore<PatFrag patStore> : PatFrag<(ops node:$val, node:$ptr), (patStore node:$val, node:$ptr), [{
		return cast<MemSDNode>(N)->getAddressSpace() == 1;
	    }]>;


defm SW  : StoreTypedPatterns<SWC, uimm7s2, immBase7s2, cachedStore<store>>;
defm SH  : StoreTypedPatterns<SHC, uimm7s1, immBase7s1, cachedStore<truncstorei16>>;
defm SB  : StoreTypedPatterns<SBC, uimm7  , immBase7  , cachedStore<truncstorei8>>;
defm SWM : StoreTypedPatterns<SWM, uimm7s2, immBase7s2, uncachedStore<store>>;
defm SHM : StoreTypedPatterns<SHM, uimm7s1, immBase7s1, uncachedStore<truncstorei16>>;
defm SBM : StoreTypedPatterns<SBM, uimm7  , immBase7  , uncachedStore<truncstorei8>>;
defm SWL : StoreTypedPatterns<SWL, uimm7s2, immBase7s2, spmStore<store>>;
defm SHL : StoreTypedPatterns<SHL, uimm7s1, immBase7s1, spmStore<truncstorei16>>;
defm SBL : StoreTypedPatterns<SBL, uimm7  , immBase7  , spmStore<truncstorei8>>;


// Branch via JumpTable

def : Pat<(brjt jtpat:$jtable, RRegs:$index),
          (BRTu (LWC (SHADD2l RRegs:$index, jtpat:$jtable), 0), jtpat:$jtable)>;

