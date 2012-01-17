//===- PatmosRegisterInfo.td - Patmos Register defs ----------*- tblgen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source 
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the Patmos register file
//===----------------------------------------------------------------------===//

class PatmosReg<bits<4> num, string n> : Register<n> {
  field bits<4> Num = num;
  let Namespace = "Patmos";
}

class PatmosRegWithSubregs<bits<4> num, string n, list<Register> subregs> 
  : RegisterWithSubRegs<n, subregs> {
  field bits<4> Num = num;
  let Namespace = "Patmos";
}

//===----------------------------------------------------------------------===//
//  Registers
//===----------------------------------------------------------------------===//

def PCB  : PatmosReg<0,  "r0">;
def SPB  : PatmosReg<1,  "r1">;
def SRB  : PatmosReg<2,  "r2">;
def CGB  : PatmosReg<3,  "r3">;
def FPB  : PatmosReg<4,  "r4">;
def R5B  : PatmosReg<5,  "r5">;
def R6B  : PatmosReg<6,  "r6">;
def R7B  : PatmosReg<7,  "r7">;
def R8B  : PatmosReg<8,  "r8">;
def R9B  : PatmosReg<9,  "r9">;
def R10B : PatmosReg<10, "r10">;
def R11B : PatmosReg<11, "r11">;
def R12B : PatmosReg<12, "r12">;
def R13B : PatmosReg<13, "r13">;
def R14B : PatmosReg<14, "r14">;
def R15B : PatmosReg<15, "r15">;

def subreg_8bit : SubRegIndex { let Namespace = "Patmos"; }

let SubRegIndices = [subreg_8bit] in {
def PCW  : PatmosRegWithSubregs<0,  "r0",  [PCB]>;
def SPW  : PatmosRegWithSubregs<1,  "r1",  [SPB]>;
def SRW  : PatmosRegWithSubregs<2,  "r2",  [SRB]>;
def CGW  : PatmosRegWithSubregs<3,  "r3",  [CGB]>;
def FPW  : PatmosRegWithSubregs<4,  "r4",  [FPB]>;
def R5W  : PatmosRegWithSubregs<5,  "r5",  [R5B]>;
def R6W  : PatmosRegWithSubregs<6,  "r6",  [R6B]>;
def R7W  : PatmosRegWithSubregs<7,  "r7",  [R7B]>;
def R8W  : PatmosRegWithSubregs<8,  "r8",  [R8B]>;
def R9W  : PatmosRegWithSubregs<9,  "r9",  [R9B]>;
def R10W : PatmosRegWithSubregs<10, "r10", [R10B]>;
def R11W : PatmosRegWithSubregs<11, "r11", [R11B]>;
def R12W : PatmosRegWithSubregs<12, "r12", [R12B]>;
def R13W : PatmosRegWithSubregs<13, "r13", [R13B]>;
def R14W : PatmosRegWithSubregs<14, "r14", [R14B]>;
def R15W : PatmosRegWithSubregs<15, "r15", [R15B]>;
}

def GR8 : RegisterClass<"Patmos", [i8], 8,
   // Volatile registers
  (add R12B, R13B, R14B, R15B, R11B, R10B, R9B, R8B, R7B, R6B, R5B,
   // Frame pointer, sometimes allocable
   FPB,
   // Volatile, but not allocable
   PCB, SPB, SRB, CGB)>;

def GR16 : RegisterClass<"Patmos", [i16], 16,
   // Volatile registers
  (add R12W, R13W, R14W, R15W, R11W, R10W, R9W, R8W, R7W, R6W, R5W,
   // Frame pointer, sometimes allocable
   FPW,
   // Volatile, but not allocable
   PCW, SPW, SRW, CGW)>
{
  let SubRegClasses = [(GR8 subreg_8bit)];
}

