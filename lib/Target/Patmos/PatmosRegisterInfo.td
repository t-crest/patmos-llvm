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

class PatmosReg<string n, list<string> altNames = []> : Register<n, altNames> {
  let Namespace = "Patmos";
}


class PatmosGPR<bits<5> num, string n> : PatmosReg<n> {
  field bits<5> Num = num;
}

class PatmosSPR<bits<4> num, string n> : PatmosReg<n> {
  field bits<4> Num = num;
}

//TODO define a subreg for the predicate register file in SZ
//     P# then are subregs of this subreg
/*
class PatmosSPRWithSubregs<bits<4> num, string n, list<PatmosReg> subregs>
  : RegisterWithSubRegs<n, subregs> {
 field bits<4> Num = num;
 let Namespace = "Patmos";
}
*/


class PatmosPRED<bits<3> num, string n> : PatmosReg<n> {
  field bits<3> Num = num;
}




//===----------------------------------------------------------------------===//
//  Registers
//===----------------------------------------------------------------------===//

def R0  : PatmosGPR< 0,  "r0">;
def R1  : PatmosGPR< 1,  "r1">;
def R2  : PatmosGPR< 2,  "r2">;
def R3  : PatmosGPR< 3,  "r3">;
def R4  : PatmosGPR< 4,  "r4">;
def R5  : PatmosGPR< 5,  "r5">;
def R6  : PatmosGPR< 6,  "r6">;
def R7  : PatmosGPR< 7,  "r7">;
def R8  : PatmosGPR< 8,  "r8">;
def R9  : PatmosGPR< 9,  "r9">;
def R10 : PatmosGPR<10, "r10">;
def R11 : PatmosGPR<11, "r11">;
def R12 : PatmosGPR<12, "r12">;
def R13 : PatmosGPR<13, "r13">;
def R14 : PatmosGPR<14, "r14">;
def R15 : PatmosGPR<15, "r15">;
def R16 : PatmosGPR<16, "r16">;
def R17 : PatmosGPR<17, "r17">;
def R18 : PatmosGPR<18, "r18">;
def R19 : PatmosGPR<19, "r19">;
def R20 : PatmosGPR<20, "r20">;
def R21 : PatmosGPR<21, "r21">;
def R22 : PatmosGPR<22, "r22">;
def R23 : PatmosGPR<23, "r23">;
def R24 : PatmosGPR<24, "r24">;
def R25 : PatmosGPR<25, "r25">;
def R26 : PatmosGPR<26, "r26">;
def R27 : PatmosGPR<27, "r27">;
def R28 : PatmosGPR<28, "r28">;
def R29 : PatmosGPR<29, "r29">;
def RFP : PatmosGPR<30, "r30">; // frame ptr
def RSP : PatmosGPR<31, "r31">; // stack ptr

def P0  : PatmosPRED<0, "p0">;
def P1  : PatmosPRED<1, "p1">;
def P2  : PatmosPRED<2, "p2">;
def P3  : PatmosPRED<3, "p3">;
def P4  : PatmosPRED<4, "p4">;
def P5  : PatmosPRED<5, "p5">;
def P6  : PatmosPRED<6, "p6">;
def P7  : PatmosPRED<7, "p7">;

// SZ (zero) instead of S0 to avoid confusion with SO
//def PRF : PatmosPRF<"pred", [P0, P1, P2, P3, P4, P5, P6, P7]>;
//def SZ  : PatmosSPRWithSubRegs< 0,  "sz", [PRF]>;
def SZ  : PatmosSPR< 0,  "sz">;
def SM  : PatmosSPR< 1,  "sm">;
def SL  : PatmosSPR< 2,  "sl">;
def SH  : PatmosSPR< 3,  "sh">;
def SB  : PatmosSPR< 4,  "sb">;
def SO  : PatmosSPR< 5,  "so">;
def ST  : PatmosSPR< 6,  "st">;
def S7  : PatmosSPR< 7,  "s7">;
def S8  : PatmosSPR< 8,  "s8">;
def S9  : PatmosSPR< 9,  "s9">;
def S10 : PatmosSPR<10, "s10">;
def S11 : PatmosSPR<11, "s11">;
def S12 : PatmosSPR<12, "s12">;
def S13 : PatmosSPR<13, "s13">;
def S14 : PatmosSPR<14, "s14">;
def S15 : PatmosSPR<15, "s15">;



def Mul : RegisterClass<"Patmos", [i32], 32, (add SL, SH)>;

def RRegs : RegisterClass<"Patmos", [i32], 32,
   // scratch registers (caller saved)
  (add R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20,
   // preserved registers (callee saved)
   R21, R22, R23, R24, R25, R26, R27, R28, R29,
   // return value
   R1, R2,
   // arguments (caller saved)
   R3, R4, R5, R6, R7, R8,
   // frame pointer, stack pointer (callee saved)
   RFP, RSP,
   // constant 0
   R0)>;

def SRegs : RegisterClass<"Patmos", [i32], 32,
  (add SZ, SM, SL, SH, SB, SO, ST, S7,
   S8, S9, S10, S11, S12, S13, S14, S15)>;

let Size = 32 in {
  def PRegs : RegisterClass<"Patmos", [i1], 32,
    // callee saved
    (add P1, P2, P3, P4, P5, P6, P7,
    // constant true
    P0)>;
}
