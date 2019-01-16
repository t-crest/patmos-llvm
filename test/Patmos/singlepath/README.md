# Single-path Code Tests

This directory contains tests of the generation of single-path code.

The scipt `assert_singlepath.sh` compiles LLVM IR programs and ensures they are valid single-path code.
It does so by using `pasim` to get statistics on the execution of the program on different input. 
It then checks the statistics are identical across executions, which is the fundamental characteristic of single-path. 
The script is intended to be used in `llvm-lit` tests, and be called as part of the `; RUN:` command in the tests. 
E.g. `; RUN: %p/assert_singlepath.sh llc %s sp_func 0=0 1=1 2=2`.

### Folders:

#### single_source

Contains single-source `llvm-lit` test programs. 
These programs are intended as the basic tests of single-path code, each testing a specific case of code generation. 

