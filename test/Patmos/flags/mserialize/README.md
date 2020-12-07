# Serialized IR/MIR output tests

This folder contains tests of the `-mserialize` flag, which makes `llc` output its internal representation of
the compiled program.

These tests are not well designed, as knowledge about the PML format is scarce.
Instead, they are simply there to try and ensure that no obvious errors are introduced to the flag
unknowingly.
With time, these tests should be substituted with proper, nonfragile tests.

_warning_: These tests are very sensitive to the programs in `../../singlepath/single_source/`. 
Any change to those programs should be expected to break the coresponding tests in this folder.

These tests were made by simply running `llc` with the flag (and no other flags) and copying the resulting
PML into the test files.

### Commands

The tests in this folder have the following commands available:

* `%check-pml`

Compiled the program in `../../singlepath/single_source` with the same name is the calling test file
using the `-mserialize` flag.
The output PML is then compared to the contents of the test file (without the `llvm-lit` commands) to
see if they are identical.
If not, the test fails.

To use this command, simply put `%check-pml` at the top command of the file:

`; RUN: %check-pml`
