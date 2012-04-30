##===- lib/Target/Patmos/Makefile --------------------------*- Makefile -*-===##
# 
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source 
# License. See LICENSE.TXT for details.
# 
##===----------------------------------------------------------------------===##

LEVEL = ../../..
LIBRARYNAME = libLLVMPatmosCodeGen
LOADABLE_MODULE = 1
TARGET = Patmos

# Make sure that tblgen is run, first thing.
BUILT_SOURCES = PatmosGenRegisterInfo.inc PatmosGenInstrInfo.inc \
		PatmosGenAsmWriter.inc \
		PatmosGenDAGISel.inc PatmosGenCallingConv.inc \
		PatmosGenSubtargetInfo.inc

DIRS = InstPrinter TargetInfo MCTargetDesc

include $(LEVEL)/Makefile.common

