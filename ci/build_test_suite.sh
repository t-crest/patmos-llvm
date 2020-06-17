#!/bin/sh -x
# Downloads and build the test suite for testing LLVM

cd build/local
# Extract LLVM artifacts
tar -xvf patmos-llvm*.tar.gz
cd ..
# Download benchmark repo
git clone https://github.com/t-crest/patmos-benchmarks bench
cd bench
git checkout 8bca6cc6490540599c7e9ec5b25816e58f611560
mkdir build
cd build
cmake .. -DCMAKE_TOOLCHAIN_FILE=../cmake/patmos-clang-toolchain.cmake -DENABLE_TESTING=true
make
cd ../../..