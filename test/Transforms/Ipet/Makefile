
# for configure+make
BUILDDIR=../../../../patmos-llvm-obj/Debug+Asserts

# for cmake+eclipse
#BUILDDIR=../../../../patmos-eclipse-obj/

CLANG=$(BUILDDIR)/bin/clang
OPT=$(BUILDDIR)/bin/opt
LINK=$(BUILDDIR)/bin/llvm-link
DIS=$(BUILDDIR)/bin/llvm-dis

-include config.mk

CLANG := $(shell if [ -e $(CLANG) ]; then echo $(CLANG); else echo clang; fi)

.PHONY: all test clean

%.bc: %.c
	$(CLANG) -O0 -emit-llvm $< -c -o $@

%.ll: %.c
	$(CLANG) -O0 -emit-llvm -S $< -o $@

%.graphs: %.ll
	$(OPT) -disable-opt -dot-cfg-only -dot-callgraph $< > /dev/null
	mkdir -p $@
	mv *.dot $@
	for DOTFILE in $@/*.dot; do dot -Tpng $$DOTFILE > $${DOTFILE%.*}.png && rm $$DOTFILE; done

main.bc: hello.ll test.ll
	$(LINK) $^ -o $@
main.ll: main.bc
	$(DIS) $<

%: %.ll
	$(OPT)  --debug-pass=Structure \
                -load $(BUILDDIR)/lib/libLLVMIpet.so -ipet \
                -loops \
                -scalar-evolution \
                -sccp -mem2reg -instcombine \
                -analyze \
                < $< > $@.out 2> $@.err

clean:
	rm -f *.bc *.ll *.out
