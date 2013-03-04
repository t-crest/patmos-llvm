PLATIN toolset
==============

This directory contains the PLATIN tools for manipulating, generating
and transforming PML files.

Requirements
------------
* ruby 1.9 or newer
* gems (see ext/gem_install.sh): rsec, ruby-graphviz, lpsolve (unofficial)

[Ubuntu]
$ sudo aptitude install ruby1.9.1-full
$ ./ext/install_gems.sh

Demos
-----

(1) Compile source code to bitcode file (not yet linked with libc)

$ mkdir src bin gen
$ patmos-clang -emit-llvm -S -o src/jumptable.bc examples/jumptable.c

(2) Compile to ELF

$ patmos-clang -o bin/jumptable.elf  -mpatmos-preemit-bitcode=bin/jumptable.elf.bc -mpatmos-serialize=bin/jumptable.elf.pml src/jumptable.bc

(3) Trace Analysis Demo (pasim trace analysis, SWEET analysis, platin IPET analysis, relation-graph roundtrips, aiT integration)

$ ./run-benchmark bin/ gen/ src/jumptable.bc bench-trace
$ ./run-benchmark bin/ gen/ src/jumptable.bc bench-sweet

Open Questions
--------------
* Currently, the LLVM machine blocks do not seem to be in order, why?
  This makes it impossible to infer missing labels, but otherwise
  is no problem.
* aiT integration:
  Is this correct ??
    "exit test at end" => loop header bound
    "exit test at beginning" => loop backedge bound = loop header bound - 1

Known Problems
--------------

* aiT infeasibility analysis
mrtc/statemate -O0: aiT reports that the loop body is infeasible, allthough it is
                    executed 3 times in the simulator trace [=> aiT bug?]
mrtc/fft1 -O0:      aiT reports that an infeasible problem, even if no annotations are
	            given [=> aiT bug?]
mrtc/lms -O0:       aiT reports infeasability [=> aiT bug?]:
	            Loop 'main.L1': dead end in first iteration in all contexts

* patmos tool chain problems
mrtc/fac -O0:       Recursion not supported
mrtc/whet -O0:      Needs math libraries => problem with atan function

* trace analsis takes > 60s
mrtc/adpcm -O0:     120s
mrtc/st -O0:        ...


Block Mapping Modifications
---------------------------
* BranchFolder: when merging the tails of two basic blocks, delete the associated bitcode BB
* BranchFolder: when merging a basic block and its successor, use one of the labels if it is defined
