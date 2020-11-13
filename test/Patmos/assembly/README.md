# Assembly Tests

This folder contains tests of Patmos assembly.

The `lit.local.cfg` file provides custom commands that can be used by `.ll` test files
for easy test setup.

### Commands

* `%test_no_runtime_execution`:

Used to test files by compiling and running them, ensuring 0 is returned.

This uses LLC and patmos-ld to compile the LLVM-IR test program without any 
standard library or runtime code of any kind, producing a standalone ELF that 
is then run on pasim.

The program must have a 'main' function defined where execution starts
and that returns 'i32'.
For correct execution, the program must return 0. Any other return value
is treated as an error and the test will fail.
The program has 400 cycles to execute before it is stopped and assumed a failure.
Note that it takes around 100 cycles to reach the 'main' function,
giving it less than 300 cycles to run. Therefore, the aim should be that the
'main' function finishes within 200 cycles.

To use this command, simply add `; RUN: %test_no_runtime_execution` to the top
of the test file.

### Folders

* `instructions`: Tests the semantics of specific instructions in inline_assembly.
* `combinations`: Tests specific combinations of instructions or syntactic elements in inline_assembly.
* `output`: Tests the output produced when LLC is asked to produce assembly (using `-filetype=asm`)