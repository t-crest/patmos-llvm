
# for configure+make
#BUILDDIR=../../../../patmos-llvm-obj/Debug+Asserts

# for cmake+eclipse
BUILDDIR=../../../../patmos-llvm-obj/

P=PATH=$(BUILDDIR)/bin

.PHONY: all test

%.bc: %.c
	$(P) clang -O0 -emit-llvm $< -c -o $@

%.ll: %.c
	$(P) clang -O0 -emit-llvm -S $< -o $@

test: hello.ll
	$(P) opt  --debug-pass=Structure \
                  -load $(BUILDDIR)/lib/libLLVMIpet.so -ipet -bbinstcnt \
                  -analyze \
                  < $< > $@.out
