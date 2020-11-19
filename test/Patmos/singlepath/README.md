# Single-path Code Tests

This directory contains tests of the generation of single-path code.

The command `%test_singlepath_execution` is provided for use in tests.
It compiles the LLVM IR program and ensures it is valid single-path code.
It does so by using `pasim` to get statistics on the execution of the program on different input. 
It then checks the statistics are identical across executions, which is the fundamental characteristic of single-path. 
The command is intended to be used in `llvm-lit` tests, and be called as part of the `; RUN:` command in the tests. 
E.g. `; RUN: %test_singlepath_execution -O2 0=0 1=1 2=2`.

After the command you must give any additional arguments to give `llc` as part of compilation.
In this example `-O2` is given. 
If more than 1 argument is needed, you must use quotes, i.e., `"-O2 -v"`.
If no arguments are needed for `llc`, `""` must be used.
After that, any number of _execution arguments_ can be given.
An execution argument is a number, followed by `=`, followed by another number.
The first number is the input to the `main` function in the tested program.
The second number must be between 0 and 256 and is the expected output of the test for that specific input.
At least 2 execution arguments must be given.

### Folders:

#### single_source

Contains single-source `llvm-lit` test programs. 
These programs are intended as the basic tests of single-path code, each testing a specific case of code generation.

