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

def R0  : PatmosGPR< 0,  "r0">, DwarfRegNum<[0]>;
def R1  : PatmosGPR< 1,  "r1">, DwarfRegNum<[1]>;
def R2  : PatmosGPR< 2,  "r2">, DwarfRegNum<[2]>;
def R3  : PatmosGPR< 3,  "r3">, DwarfRegNum<[3]>;
def R4  : PatmosGPR< 4,  "r4">, DwarfRegNum<[4]>;
def R5  : PatmosGPR< 5,  "r5">, DwarfRegNum<[5]>;
def R6  : PatmosGPR< 6,  "r6">, DwarfRegNum<[6]>;
def R7  : PatmosGPR< 7,  "r7">, DwarfRegNum<[7]>;
def R8  : PatmosGPR< 8,  "r8">, DwarfRegNum<[8]>;
def R9  : PatmosGPR< 9,  "r9">, DwarfRegNum<[9]>;
def R10 : PatmosGPR<10, "r10">, DwarfRegNum<[10]>;
def R11 : PatmosGPR<11, "r11">, DwarfRegNum<[11]>;
def R12 : PatmosGPR<12, "r12">, DwarfRegNum<[12]>;
def R13 : PatmosGPR<13, "r13">, DwarfRegNum<[13]>;
def R14 : PatmosGPR<14, "r14">, DwarfRegNum<[14]>;
def R15 : PatmosGPR<15, "r15">, DwarfRegNum<[15]>;
def R16 : PatmosGPR<16, "r16">, DwarfRegNum<[16]>;
def R17 : PatmosGPR<17, "r17">, DwarfRegNum<[17]>;
def R18 : PatmosGPR<18, "r18">, DwarfRegNum<[18]>;
def R19 : PatmosGPR<19, "r19">, DwarfRegNum<[19]>;
def R20 : PatmosGPR<20, "r20">, DwarfRegNum<[20]>;
def R21 : PatmosGPR<21, "r21">, DwarfRegNum<[21]>;
def R22 : PatmosGPR<22, "r22">, DwarfRegNum<[22]>;
def R23 : PatmosGPR<23, "r23">, DwarfRegNum<[23]>;
def R24 : PatmosGPR<24, "r24">, DwarfRegNum<[24]>;
def R25 : PatmosGPR<25, "r25">, DwarfRegNum<[25]>;
def R26 : PatmosGPR<26, "r26">, DwarfRegNum<[26]>;
def R27 : PatmosGPR<27, "r27">, DwarfRegNum<[27]>;
def R28 : PatmosGPR<28, "r28">, DwarfRegNum<[28]>;
def RTR : PatmosGPR<29, "r29">, DwarfRegNum<[29]>; // reserved temporary register
def RFP : PatmosGPR<30, "r30">, DwarfRegNum<[30]>; // frame ptr
def RSP : PatmosGPR<31, "r31">, DwarfRegNum<[31]>; // stack ptr

def P0  : PatmosPRED<0, "p0">, DwarfRegNum<[50]>;
def P1  : PatmosPRED<1, "p1">, DwarfRegNum<[51]>;
def P2  : PatmosPRED<2, "p2">, DwarfRegNum<[52]>;
def P3  : PatmosPRED<3, "p3">, DwarfRegNum<[53]>;
def P4  : PatmosPRED<4, "p4">, DwarfRegNum<[54]>;
def P5  : PatmosPRED<5, "p5">, DwarfRegNum<[55]>;
def P6  : PatmosPRED<6, "p6">, DwarfRegNum<[56]>;
def P7  : PatmosPRED<7, "p7">, DwarfRegNum<[57]>;

//def PRF : PatmosPRF<"pred", [P0, P1, P2, P3, P4, P5, P6, P7]>;
//def SZ  : PatmosSPRWithSubRegs< 0,  "sz", [PRF]>;

let Aliases = [P0, P1, P2, P3, P4, P5, P6, P7] in
def S0  : PatmosSPR< 0,  "s0">, DwarfRegNum<[0]>; // zero

def SM  : PatmosSPR< 1,  "s1">, DwarfRegNum<[32]>;
def SL  : PatmosSPR< 2,  "s2">, DwarfRegNum<[33]>;
def SH  : PatmosSPR< 3,  "s3">, DwarfRegNum<[34]>;
def S4  : PatmosSPR< 4,  "s4">, DwarfRegNum<[35]>;
def SS  : PatmosSPR< 5,  "s5">, DwarfRegNum<[36]>;
def ST  : PatmosSPR< 6,  "s6">, DwarfRegNum<[37]>;
def SRB : PatmosSPR< 7,  "s7">, DwarfRegNum<[38]>;
def SRO : PatmosSPR< 8,  "s8">, DwarfRegNum<[39]>;
def SXB : PatmosSPR< 9,  "s9">, DwarfRegNum<[40]>;
def SXO : PatmosSPR<10, "s10">, DwarfRegNum<[41]>;
def S11 : PatmosSPR<11, "s11">, DwarfRegNum<[42]>;
def S12 : PatmosSPR<12, "s12">, DwarfRegNum<[43]>;
def S13 : PatmosSPR<13, "s13">, DwarfRegNum<[44]>;
def S14 : PatmosSPR<14, "s14">, DwarfRegNum<[45]>;
def S15 : PatmosSPR<15, "s15">, DwarfRegNum<[46]>;



def Mul : RegisterClass<"Patmos", [i32], 32, (add SL, SH)>;

// Note: the order of the registers must be kept consistent in PatmosBaseInfo.h and
// in the disassembler.

def RRegs : RegisterClass<"Patmos", [i32], 32,
   // constant 0
  (add R0,
   // return value
   R1, R2,
   // arguments (caller saved)
   R3, R4, R5, R6, R7, R8,
   // scratch registers (caller saved)
   R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20,
   // preserved registers (callee saved)
   R21, R22, R23, R24, R25, R26, R27, R28,
   // temp register
   RTR,
   // frame pointer, stack pointer (callee saved)
   RFP, RSP)>;

def SRegs : RegisterClass<"Patmos", [i32], 32,
  (add S0, SM, SL, SH, S4, SS, ST, SRB,
   SRO, SXB, SXO, S11, S12, S13, S14, S15)>;

let Size = 32 in {
  def PRegs : RegisterClass<"Patmos", [i1], 32,
    // constant true
    (add P0,
    // callee saved
    P1, P2, P3, P4, P5, P6, P7)>;
}
