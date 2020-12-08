# Assembly Tests

This folder contains tests of the Patmos target.

The `lit.local.cfg` file provides custom commands that can be used by `.ll` test files
for easy test setup.

### Folders

* `assembly`: Tests relating to inline assembly or reading or writing assembly.
* `flags`: Tests various Patmos-only flags (and combinations).
* `llvm-lit_commands`: Tests for the `llvm-lit` commands provided to all tests.
* `optimization`: Tests various optimizations the compiler can/does do.
* `singlepath`: Tests single-path code generation.

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
The program has 800 cycles to execute before it is stopped and assumed a failure.
Note that it takes around 100 cycles to reach the 'main' function,
giving it less than 700 cycles to run. Therefore, the aim should be that the
'main' function finishes within 600 cycles.

To use this command, 
simply add `; RUN: %test_no_runtime_execution` to the top of the test file.

To customize the compilation and execution, 
the following arguments can be (optionally) set:

* `LLC_ARGS`: Additional arguments to give `llc` when compiling.
* `LD_ARGS`: Additional arugments to given the linker when doing the final linking.
* `PASIM_ARGS`: Additional arguments to give `pasim` when executing.

Arguments must be set as environmental variables before the command:

`; RUN: LLC_ARGS="-O2"; PASIM_ARGS="-D ideal"; %test_no_runtime_execution`

This command test produces a compiled binary in `%t`. 
However, `%t` is not used during the compilation,
which makes it available to use in the test before the compilation is finished.

* `%XFAIL-filecheck`:

Like `XFAIL`, ensures that the preceding command failed, 
but also runs `FileCheck` on the `stderr` output to see if it matches the expected.

The command should be positioned directly after the command to be tested, 
and followed by the rest of the arguments to give to `FileCheck` 
(e.g. the file to use to check):

`( >&2 echo "Some stderr output"; false ) %XFAIL-filecheck %s`

To check the `stdout` output instead of `stderr`, 
the `CHECK_STDOUT` argument can be (optionally) set as an environmental variables to `true`:

`CHECK_STDOUT=true; ( echo "Some stdout output"; false ) %XFAIL-filecheck %s`
