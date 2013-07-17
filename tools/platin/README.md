PLATIN toolset
==============

This directory contains the PLATIN tools for manipulating, generating
and transforming PML files.

Requirements
------------

* ruby 1.9 or newer (mandatory)
  - Ubuntu 12.10
    sudo aptitue install ruby1.9.1, ruby1.9.1-dev
  - gems will be installed automatically if necessary (rsec, ruby-graphviz, lpsolve)


Basic Usage
-----------

* Compile test program
    echo 'volatile int out; __attribute__((noinline)) void f(int x) { int i; for(i = 0; i<1024;i++) out+=x; } ' > test.c
    echo 'int main() { f(3); f(5); return 0; }' >> test.c
    patmos-clang -Wall -mserialize=test.pml -mpreemit-bitcode=test.bc -o test test.c

* Analyze using aiT

    platin wcet -i test.pml -b test --report

* Analyze f() using platin-internal analyzer only

    platin wcet --analysis-entry f -i test.pml -b test --disable-ait --enable-wca --report

* Enable analysis of the (dynamic) execution trace [for comparison]

    platin wcet --enable-trace-analysis  --trace-entry main --analysis-entry f -i test.pml -b test --enable-wca --report

* Use flow facts from the (dynamic) execution trace [for early-stage development]

    platin wcet --use-trace-facts  --trace-entry main --analysis-entry f -i test.pml -b test --enable-wca --report


Demo of individual tools
------------------------

MORE TO COME

(1) Context-sensitive trace analysis

    platin analyze-trace -i bin/jumptable.elf.pml bin/jumptable.elf --callstring-length 1 --analysis-entry=main


Known Problems
--------------

* tool chain problems
mrtc/whet -O0:      Needs math libraries => problem with atan function


LLVM Integration
================

Options
-------
CodeGen/Passes: -mserialize, -mserialize-roots, -mpreemit-bitcode

Architectures
=============

Notes for ARM:

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
