//===- ELF.cpp - ELF object file implementation -----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/Object/ELF.h"

namespace llvm {
namespace object {

#define ELF_RELOC(name, value)                                          \
  case ELF::name:                                                       \
    return #name;                                                       \

StringRef getELFRelocationTypeName(uint32_t Machine, uint32_t Type) {
  switch (Machine) {
  case ELF::EM_PATMOS:
    switch (Type) {
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_NONE);
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_CFLI_ABS);
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_ALUI_ABS);
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_ALUL_ABS);
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_MEMB_ABS);
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_MEMH_ABS);
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_MEMW_ABS);
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_ABS_32);
      LLVM_ELF_SWITCH_RELOC_TYPE_NAME(R_PATMOS_CFLI_PCREL);
    default:
      break;
    }
  case ELF::EM_X86_64:
    switch (Type) {
#include "llvm/Support/ELFRelocs/x86_64.def"
    default:
      break;
    }
    break;
  case ELF::EM_386:
    switch (Type) {
#include "llvm/Support/ELFRelocs/i386.def"
    default:
      break;
    }
    break;
  case ELF::EM_MIPS:
    switch (Type) {
#include "llvm/Support/ELFRelocs/Mips.def"
    default:
      break;
    }
    break;
  case ELF::EM_AARCH64:
    switch (Type) {
#include "llvm/Support/ELFRelocs/AArch64.def"
    default:
      break;
    }
    break;
  case ELF::EM_ARM:
    switch (Type) {
#include "llvm/Support/ELFRelocs/ARM.def"
    default:
      break;
    }
    break;
  case ELF::EM_HEXAGON:
    switch (Type) {
#include "llvm/Support/ELFRelocs/Hexagon.def"
    default:
      break;
    }
    break;
  case ELF::EM_PPC:
    switch (Type) {
#include "llvm/Support/ELFRelocs/PowerPC.def"
    default:
      break;
    }
    break;
  case ELF::EM_PPC64:
    switch (Type) {
#include "llvm/Support/ELFRelocs/PowerPC64.def"
    default:
      break;
    }
    break;
  case ELF::EM_S390:
    switch (Type) {
#include "llvm/Support/ELFRelocs/SystemZ.def"
    default:
      break;
    }
    break;
  case ELF::EM_SPARC:
  case ELF::EM_SPARC32PLUS:
  case ELF::EM_SPARCV9:
    switch (Type) {
#include "llvm/Support/ELFRelocs/Sparc.def"
    default:
      break;
    }
    break;
  default:
    break;
  }
  return "Unknown";
}

#undef ELF_RELOC

} // end namespace object
} // end namespace llvm
