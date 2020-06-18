#!/bin/sh -x
# Runs the test suite for testing LLVM

BENCH_DIR=build/bench
BENCH_BUILD_DIR=$BENCH_DIR/build

CORE_COUNT=$(grep -c ^processor /proc/cpuinfo)

cd $BENCH_BUILD_DIR
ctest -j$CORE_COUNT
cd -
rm -rf $BENCH_DIR