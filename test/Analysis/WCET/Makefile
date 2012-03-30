
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

.PHONY: all clean
.SUFFIXES:

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
#	rm -f $@.out $@.err
#	($(OPT)  --debug-pass=Structure --debug-only=ipet \
#                -load $(BUILDDIR)/lib/libLLVMWCET.so -mem2reg -ipet \
#                -analyze -dot-cfg \
#                < $< | tee -a $@.out) 2>&1 | tee $@.err | tee -a $@.out 1>&2
	$(OPT)  --debug-pass=Structure --debug-only=ipet \
                 -load $(BUILDDIR)/lib/libLLVMWCET.so -mem2reg -ipet \
                 -analyze -dot-cfg -time-passes $<

ipet: main.ll
	$(OPT) --debug-pass=Structure --debug-only=ipet -load $(BUILDDIR)/lib/libLLVMWCET.so -instnamer -ipet -analyze -dot-callgraph -dot-cfg  main.ll

clean:
	rm -f *.bc *.ll *.out
