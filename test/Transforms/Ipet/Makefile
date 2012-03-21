
# for configure+make
#BUILDDIR=../../../../patmos-llvm-obj/Debug+Asserts
#LIB=$(BUILDDIR)/lib/libLLVMIpet.so

# for cmake+make
BUILDDIR=../../../../patmos-llvm-obj/
LIB=$(BUILDDIR)/lib/Ipet.so

P=PATH=$(BUILDDIR)/bin

.PHONY: all test

%.bc: %.c
	$(P) clang -O0 -emit-llvm $< -c -o $@

%.ll: %.c
	$(P) clang -O0 -emit-llvm -S $< -o $@

test: hello.ll
	$(P) opt  --debug-pass=Structure \
                  -load $(LIB) -ipet -bbinstcnt \
                  -analyze \
                  < $< > $@.out
