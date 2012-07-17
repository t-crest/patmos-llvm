
Low Level Virtual Machine (LLVM) - Patmos Development Branch
============================================================

This directory and its subdirectories contain source code for the Low Level
Virtual Machine, a toolkit for the construction of highly optimized compilers,
optimizers, and runtime environments.

This repository contains the LLVM backend for the Patmos processor.

LLVM is open source software. You may freely distribute it under the terms of
the license agreement found in LICENSE.txt.

Please see the HTML documentation provided in docs/index.html for further
assistance with LLVM.

If you're writing a package for LLVM, see docs/Packaging.html for our
suggestions.


Toolchain
---------

For more information about the Patmos backend (ELF file format, ..), please refer to
lib/Target/Patmos/README.txt

From the github.com/t-crest organization, you need to check out the following 
additional repositories:

- patmos-clang as tools/clang for the C frontend
- patmos-newlib and patmos-compiler-rt into separate directories, for libc and 
  runtime libraries
- patmos-gold for the linker

See the README files in the various repositories for information on how to build 
them.

