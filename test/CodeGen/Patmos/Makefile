
BUILDDIR=../../../Debug+Asserts/

# to override default BUILDDIR
-include config.mk

LLC=$(BUILDDIR)/bin/llc -load $(BUILDDIR)/lib/libLLVMPatmosCodeGen.so -march=patmos
CLANG=$(BUILDDIR)/bin/clang

VIEW= \
  -view-dag-combine1-dags \
  -view-legalize-dags \
  -view-dag-combine2-dags \
  -view-isel-dags \
  -view-sched-dags \
  -view-sunit-dags


.PHONY: all
.PRECIOUS: %.ll

all: return.s

%.ll: %.c
	$(CLANG) -O0 -S -emit-llvm -o $@ $<

%.s: %.ll
	$(LLC) -O0 $(VIEW) -o $@ $<
	@echo
