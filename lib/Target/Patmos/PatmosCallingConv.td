//==- PatmosCallingConv.td - Calling Conventions for Patmos -*- tablegen -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for Patmos architecture.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Patmos Return Value Calling Convention
//===----------------------------------------------------------------------===//
def RetCC_Patmos : CallingConv<[
  // Promote i1/i8/i16 arguments to i32
  CCIfType<[i1, i8, i16], CCPromoteToType<i32>>,

  // i32 are returned in registers R1, R2
  CCIfType<[i32], CCAssignToReg<[R1, R2]>>
  // TODO return i64 in R1 and R2
]>;

//===----------------------------------------------------------------------===//
// Patmos Argument Calling Conventions
//===----------------------------------------------------------------------===//
def CC_Patmos : CallingConv<[
  // Promote i1/i8/i16 arguments to i32.
  CCIfType<[i1, i8, i16], CCPromoteToType<i32>>,

  // The first 6 integer arguments of non-varargs functions are passed in
  // integer registers.
  CCIfNotVarArg<CCIfType<[i32], CCAssignToReg<[R3, R4, R5, R6, R7, R8]>>>,

  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;
