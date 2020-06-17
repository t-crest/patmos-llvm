#!/bin/sh -x
# Build LLVM and runs automatic tests

# Download clang
cd tools
git clone https://github.com/t-crest/patmos-clang/ clang
cd clang
git checkout 73474a8ae98f1a281a03c440a4d8b9987029cf4e
cd ../..

# Build LLVM
cd build
cmake .. -DCMAKE_CXX_STANDARD=14 -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=local -DLLVM_TARGETS_TO_BUILD=Patmos -DLLVM_DEFAULT_TARGET_TRIPLE=patmos-unknown-unknown-elf -DCLANG_ENABLE_ARCMT=false -DCLANG_ENABLE_STATIC_ANALYZER=false -DCLANG_ENABLE_TESTS=false -DCLANG_ENABLE_DOCS=false -DCLANG_BUILD_EXAMPLES=false
make UnitTests llc llvm-link clang llvm-config llvm-objdump opt 

# Build newlib
git clone https://github.com/t-crest/patmos-newlib
cd patmos-newlib
git checkout 4c149a53f8cb2478d99aac731b61b5e4ed63543f
mkdir build
cd build
../configure  --target=patmos-unknown-unknown-elf AR_FOR_TARGET=ar RANLIB_FOR_TARGET=ranlib LD_FOR_TARGET=ld CC_FOR_TARGET=$TRAVIS_BUILD_DIR/build/bin/clang CFLAGS_FOR_TARGET="-target patmos-unknown-unknown-elf -O3" --prefix=$TRAVIS_BUILD_DIR/build/local
# We use 'MAKEINFO=true' to avoid building documentation
make -j MAKEINFO=true
make -j MAKEINFO=true install
cd ../..

# Run tests
env DEBUG_TYPE="" LINK_LIBS=$TRAVIS_BUILD_DIR/build/local/patmos-unknown-unknown-elf/lib ./bin/llvm-lit ../test --filter=Patmos -v

# Prepare release artifact and put it in cache
make box
mv patmos-llvm*.tar.gz local/
cd ..

