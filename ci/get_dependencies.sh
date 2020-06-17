#!/bin/sh -x
# Downloads and extract all dependencies necessary to build and test LLVM

mkdir -p build/local
cd build/local
# Empty cache from last run (no-op if no cache)
rm -rf *
# Dowload Dependencies (compiler-rt, gold, patmos-simulator)
wget -O patmos-compiler-rt.tar.gz "https://github.com/t-crest/patmos-compiler-rt/releases/download/v1.0.0-rc-1/patmos-compiler-rt-v1.0.0-rc-1.tar.gz"
wget -O patmos-gold.tar.gz "https://github.com/t-crest/patmos-gold/releases/download/v1.0.0-rc-1/patmos-gold-v1.0.0-rc-1.tar.gz"
wget -O patmos-simulator.tar.gz "https://github.com/t-crest/patmos-simulator/releases/download/1.0.0/patmos-simulator-x86_64-linux-gnu.tar.gz"
# Extract dependencies
tar -xvf patmos-compiler-rt.tar.gz
tar -xvf patmos-gold.tar.gz
tar -xvf patmos-simulator.tar.gz
cd ../..
# Delete benchmark if present
rm -rf build/bench