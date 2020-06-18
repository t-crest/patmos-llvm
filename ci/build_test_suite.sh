#!/bin/sh -x
# Downloads and build the test suite for testing LLVM
set -e

BENCH_LINK=https://github.com/t-crest/patmos-benchmarks

BENCH_COMMIT=8bca6cc6490540599c7e9ec5b25816e58f611560

PATH_DIR=build/local

cd $PATH_DIR
# Extract LLVM artifacts
tar -xvf patmos-llvm*.tar.gz
cd ..
# Download benchmark repo
git clone $BENCH_LINK bench
cd bench
git checkout $BENCH_COMMIT
mkdir build
cd build
cmake .. -DCMAKE_TOOLCHAIN_FILE=../cmake/patmos-clang-toolchain.cmake -DENABLE_TESTING=true
make
cd ../../..