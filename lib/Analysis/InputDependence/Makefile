##===- lib/Transforms/TGSA/Makefile -----------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

# Path to top level of LLVM hierarchy
LEVEL = ../../..

# Name of the library to build
LIBRARYNAME = PIDDA

# Make the shared library become a loadable module so the tools can 
# dlopen/dlsym on the resulting library.
LOADABLE_MODULE = 1

CFLAGS="-DDEBUG_CONTROL_DEPS"

# Include the makefile implementation stuff
include $(LEVEL)/Makefile.common

# EXPORTED_SYMBOL_FILE = $(PROJ_SRC_DIR)/TGSA.exports
# ifneq ($(REQUIRES_RTTI), 1)
# ifneq ($(REQUIRES_EH), 1)
# EXPORTED_SYMBOL_FILE = $(PROJ_SRC_DIR)/TGSA.exports
# endif
# endif

