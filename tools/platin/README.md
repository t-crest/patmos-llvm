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


Demo: Automated Benchmark Evaluation
------------------------------------

$ SETS="basic" ./eval-run

Demo with manual compilation: platin, aiT, SWEET
------------------------------------------------

(1) Compile source code to bitcode file (not yet linked with libc)

$ mkdir src bin gen
$ patmos-clang -emit-llvm -S -o src/jumptable.bc examples/jumptable.c

(2) Compile to ELF

$ patmos-clang -o bin/jumptable.elf \
               -mpatmos-preemit-bitcode=bin/jumptable.elf.bc \
               -mserialize=bin/jumptable.elf.pml src/jumptable.bc

(3) Trace Analysis Demo (platin trace analysis, platin IPET analysis, relation-graph roundtrips, aiT integration)

$ ./platin bench-trace --outdir gen --binary bin/jumptable.elf -o gen/jumptable.pml bin/jumptable.elf.pml

(3b) Trace Analysis Demo + SWEET bitcode analysis (requires AlfBackend and SWEET)

$ ./platin bench-sweet --outdir gen --bitcode bin/jumptable.elf.bc \
                       --binary bin/jumptable.elf -o gen/jumptable.pml bin/jumptable.elf.pml

Demo of individual tools
------------------------

(1) Context-sensitive trace analysis

$ ./platin analyze-trace -i bin/jumptable.elf.pml bin/jumptable.elf --callstring-length 1 --analysis-entry=main


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

LLVM Integration
================

Options
-------
CodeGen/Passes: -mserialize, -mserialize-roots
Patmos specific: -mpatmos-preemit-bitcode

Block Mapping Modifications
---------------------------
* BranchFolder: when merging the tails of two basic blocks, delete the associated bitcode BB
* BranchFolder: when merging a basic block and its successor, use one of the labels if it is defined

Architectures
=============

ARM:
    # Install ARM crosscompiler (Ubuntu)
    #   sudo aptitude install gcc-arm-linux-gnueabi  libc6-dev-armel-cross
    #
    # Install ARM simulator (Ubuntu)
    #   hg clone http://repo.gem5.org/gem5-stable
    #   cd gem5-stable
    #   emacs src/arch/arm/linux/process.cc # change linux version in line 69 to '2.6.35'
    #   scons build/ARM/gem5.opt
    #   GEM5_HOME=`pwd`

    # Demo
    # source = src/$1.c
    mkdir -p bin out

    # Compile using clang
    patmos-clang -O1 -target arm-linux-gnueabi -marm -Xclang -backend-option -Xclang -mserialize=bin/$1.pml \
                     -c -o bin/$1.o  src/$1.c

    # Link using arm-linux-gnueabi-gcc
    arm-linux-gnueabi-gcc  -static bin/$1.o -o bin/$1.elf

    # Simulate using GEM5
    $GEM5_HOME/build/ARM/gem5.opt --debug-flags=Exec,-ExecMicro,ExecMacro --trace-file=$1.trace \
                                  $GEM5_HOME/configs/example/se.py -c bin/$1.elf

    # Analyze using platin
    platin bench-trace --disable-ait --trace-file m5out/$1.trace --outdir out -o out/$1.pml --binary bin/$1.elf bin/$1.pml
