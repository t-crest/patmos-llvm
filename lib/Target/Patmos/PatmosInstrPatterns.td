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

// Return
//def : Pat<(retflag ),(RET )>;

// Branch instructions

def : Pat<(br bb:$target),   (BC  bb:$target)>;
def : Pat<(brind RRegs:$rs), (BCR RRegs:$rs)>;
