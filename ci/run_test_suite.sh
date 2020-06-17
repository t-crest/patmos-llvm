#!/bin/sh -x
# Runs the test suite for testing LLVM

cd build/bench/build
ctest -j$(grep -c ^processor /proc/cpuinfo)
cd ../../..
rm -rf build/bench