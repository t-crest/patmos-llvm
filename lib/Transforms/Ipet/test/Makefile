
BUILDDIR=../../../../Debug+Asserts
P=PATH=$(BUILDDIR)/bin

.PHONY: all test

%.bc: %.c
	$(P) clang -O0 -emit-llvm $< -c -o $@

%.ll: %.c
	$(P) clang -O0 -emit-llvm -S $< -o $@

test: hello.ll
	$(P) opt  --debug-pass=Structure \
                  -load $(BUILDDIR)/lib/libLLVMIpet.so -bbinstcnt \
                  -analyze \
                  < $< > /dev/null
