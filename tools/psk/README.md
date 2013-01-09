PSK (Patmos Swiss Knife)
========================

This directory contains tools for manipulating, generating and transforming
PML files.

Open Questions:
* Do we want to stick with PSK/PML terminology?

Benchmark Analysis Demo
-----------------------

(1) compile (=> elf) and generate a pml file [clang]
(2) simulate, analyze trace and generate flow facts for benchmark
(3) export trace flow facts (if necessary) and jumptables to .ais format
(4) run aiT
