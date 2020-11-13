#!/bin/sh -x
# Build LLVM and runs automatic tests
set -e

CLANG_LINK=https://github.com/t-crest/patmos-clang/
NEWLIB_LINK=https://github.com/t-crest/patmos-newlib/

CLANG_COMMIT=73474a8ae98f1a281a03c440a4d8b9987029cf4e
NEWLIB_COMMIT=4c149a53f8cb2478d99aac731b61b5e4ed63543f

PATMOS_TRIPLE=patmos-unknown-unknown-elf

BUILD_DIR=$TRAVIS_BUILD_DIR/build
PATH_DIR=$BUILD_DIR/local

# Download clang
cd tools
git clone $CLANG_LINK clang
cd clang
git checkout $CLANG_COMMIT
cd ../..

# Build LLVM
cd build
cmake .. -DCMAKE_CXX_STANDARD=14 -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=local -DLLVM_TARGETS_TO_BUILD=Patmos -DLLVM_DEFAULT_TARGET_TRIPLE=$PATMOS_TRIPLE -DCLANG_ENABLE_ARCMT=false -DCLANG_ENABLE_STATIC_ANALYZER=false -DCLANG_ENABLE_TESTS=false -DCLANG_ENABLE_DOCS=false -DCLANG_BUILD_EXAMPLES=false
make UnitTests llc llvm-link clang llvm-config llvm-objdump opt FileCheck

# Build newlib
git clone $NEWLIB_LINK
cd patmos-newlib
git checkout $NEWLIB_COMMIT
mkdir build
cd build
../configure  --target=$PATMOS_TRIPLE AR_FOR_TARGET=ar RANLIB_FOR_TARGET=ranlib LD_FOR_TARGET=ld CC_FOR_TARGET=$BUILD_DIR/bin/clang CFLAGS_FOR_TARGET="-target $PATMOS_TRIPLE -O3" --prefix=$PATH_DIR
# We use 'MAKEINFO=true' to avoid building documentation
make -j MAKEINFO=true
make -j MAKEINFO=true install
cd ../..

# Run tests
env DEBUG_TYPE="" LINK_LIBS=$PATH_DIR/$PATMOS_TRIPLE/lib ./bin/llvm-lit ../test --filter=Patmos -v

# Prepare release artifact and put it in cache
make box
mv patmos-llvm*.tar.gz local/
cd ..

