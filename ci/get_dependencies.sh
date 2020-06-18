#!/bin/sh -x
# Downloads and extract all dependencies necessary to build and test LLVM
set -e

PATH_DIR=build/local
BENCH_DIR=build/bench

COMPILER_RT_TAR=patmos-compiler-rt.tar.gz
GOLD_TAR=patmos-gold.tar.gz
SIMULAROT_TAR=patmos-simulator.tar.gz

COMPILER_RT_LINK=https://github.com/t-crest/patmos-compiler-rt/releases/download/v1.0.0-rc-1/patmos-compiler-rt-v1.0.0-rc-1.tar.gz
GOLD_LINK=https://github.com/t-crest/patmos-gold/releases/download/v1.0.0-rc-1/patmos-gold-v1.0.0-rc-1.tar.gz
SIMULATOR_LINK=https://github.com/t-crest/patmos-simulator/releases/download/1.0.0/patmos-simulator-x86_64-linux-gnu.tar.gz

mkdir -p $PATH_DIR
cd $PATH_DIR

# Empty Travis-CI cache from last run (no-op if no cache)
rm -rf *
rm -rf $BENCH_DIR

# Dowload Dependencies (compiler-rt, gold, patmos-simulator)
wget -O $COMPILER_RT_TAR $COMPILER_RT_LINK
wget -O $GOLD_TAR $GOLD_LINK
wget -O $SIMULAROT_TAR $SIMULATOR_LINK

# Extract dependencies
tar -xvf $COMPILER_RT_TAR
tar -xvf $GOLD_TAR
tar -xvf $SIMULAROT_TAR
cd -

