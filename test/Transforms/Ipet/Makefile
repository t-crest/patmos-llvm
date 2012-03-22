
# for configure+make
#BUILDDIR=../../../../patmos-llvm-obj/Debug+Asserts

# for cmake+eclipse
BUILDDIR=../../../../patmos-llvm-obj/

#P=PATH=$(BUILDDIR)/bin:$(PATH)

CLANG=$(BUILDDIR)/bin/clang
OPT=$(BUILDDIR)/bin/opt
LINK=$(BUILDDIR)/bin/llvm-link
DIS=$(BUILDDIR)/bin/llvm-dis

.PHONY: all test

%.bc: %.c
	$(CLANG) -O0 -emit-llvm $< -c -o $@

%.ll: %.c
	$(CLANG) -O0 -emit-llvm -S $< -o $@

main.bc: hello.ll test.ll
	$(LINK) $^ -o $@
main.ll: main.bc
	$(DIS) $<

test: main.ll
	$(OPT)  --debug-pass=Structure \
                -load $(BUILDDIR)/lib/libLLVMIpet.so -ipet -bbinstcnt \
                -analyze \
                < $< > $@.out
