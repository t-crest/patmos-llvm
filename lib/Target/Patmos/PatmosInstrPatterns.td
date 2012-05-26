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



// load immediate
def : Pat<(i32 imm:$imm),
          (LIi (i32 uimm12:$imm) )>;

def : Pat<(i32 imm:$imm),
          (LIl (i32 imm:$imm) )>;

// Return
//def : Pat<(retflag ),(RET )>;

// Branch instructions

def : Pat<(br bb:$target),   (BC  bb:$target)>;
def : Pat<(brind RRegs:$rs), (BCR RRegs:$rs)>;

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






// load patterns


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
defm LW  : LoadTypedPatterns<LWC , imm7s2, load>;
defm LH  : LoadTypedPatterns<LHC , imm7s1, sextloadi16>;
defm LB  : LoadTypedPatterns<LBC , imm7  , sextloadi8>;
defm LHU : LoadTypedPatterns<LHUC, imm7s1, zextloadi16>;
defm LBU : LoadTypedPatterns<LBUC, imm7  , zextloadi8>;




// store patterns


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
defm SW  : StoreTypedPatterns<SWC, imm7s2, store>;
defm SH  : StoreTypedPatterns<SHC, imm7s1, truncstorei16>;
defm SB  : StoreTypedPatterns<SBC, imm7  , truncstorei8>;

