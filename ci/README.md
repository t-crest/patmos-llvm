Continuous Integration scripts
====================

These scripts are meant to be run by Travis-CI for testing and deploying the project binaries.
However, they can also be used to build/test the project locally.
These scripts are meant to be run from the project root folder (`cd ..` from this folder.)

The scripts should be run in the following order:

- `get_dependencies`
- `build_test_llvm`
- `build_test_suite`
- `run_test_suite`

Running them in a different order or skipping any of them may result in errors.

