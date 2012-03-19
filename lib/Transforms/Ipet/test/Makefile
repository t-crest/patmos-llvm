
BUILDDIR=../../../../Debug+Asserts
P=PATH=$(BUILDDIR)/bin

.PHONY: all test

%.bc: %.c
	$(P) clang -O0 -emit-llvm $< -c -o $@

test: hello.bc
	$(P) opt -load $(BUILDDIR)/lib/Ipet.so -ipet2 < hello.bc > /dev/null
