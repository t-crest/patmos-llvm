##===- lib/Target/Patmos/Makefile --------------------------*- Makefile -*-===##
# 
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source 
# License. See LICENSE.TXT for details.
# 
##===----------------------------------------------------------------------===##

LEVEL = ../../..
LIBRARYNAME = LLVMPatmosCodeGen
#LOADABLE_MODULE = 1
TARGET = Patmos

# Make sure that tblgen is run, first thing.
BUILT_SOURCES = PatmosGenRegisterInfo.inc PatmosGenInstrInfo.inc \
		PatmosGenAsmWriter.inc \
		PatmosGenDAGISel.inc PatmosGenCallingConv.inc \
		PatmosGenSubtargetInfo.inc \
		PatmosGenCodeEmitter.inc PatmosGenMCCodeEmitter.inc \
		PatmosGenEDInfo.inc PatmosGenDisassemblerTables.inc

DIRS = InstPrinter TargetInfo MCTargetDesc AsmParser Disassembler

include $(LEVEL)/Makefile.common

