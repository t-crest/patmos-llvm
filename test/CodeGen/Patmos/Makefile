
BUILDDIR=../../../Debug+Asserts/

ON?=2
ARCH ?= patmos

GDB ?=
ifeq ($(GDB),1)
  GDB = gdb --args
endif

LLCBIN=$(BUILDDIR)/bin/llc
MCBIN=$(BUILDDIR)/bin/llvm-mc

LLC=$(LLCBIN) -march=$(ARCH)
OBJDUMP=$(BUILDDIR)/bin/llvm-objdump
MC=$(MCBIN) -arch=$(ARCH)

CLANG=$(BUILDDIR)/bin/clang
OPT=$(BUILDDIR)/bin/opt

# to override default BUILDDIR
-include config.mk


VIEW= \
  -view-dag-combine1-dags \
  -view-legalize-types-dags \
  -view-legalize-dags \
  -view-dag-combine2-dags \
  -view-dag-combine-lt-dags \
  -view-isel-dags \
  -view-sched-dags \
  -view-sunit-dags

VIEW= \
  -view-legalize-dags \
  -view-isel-dags \
  -view-sched-dags

VIEW=

DEBUG=--debug

.PHONY: all clean
.PRECIOUS: %.ll

all: return.s

%.ll: %.c
	$(CLANG) -O$(ON) -S -emit-llvm -o $@ $<

%.s: %.ll $(LLCBIN)
	$(GDB) $(LLC) -O0 $(DEBUG) $(VIEW) -show-mc-encoding -o $@ $<
	./hex2bin -i $@
	@echo

%.o: %.ll $(LLCBIN)
	$(LLC) -O0 -filetype=obj -o $@ $<

%.as: %.s $(MCBIN)
	$(MC) -filetype=obj -assemble -o $@ $<

%.dis: %.o $(MCBIN)
	$(OBJDUMP) -disassemble $<

clean:
	rm -f *.s *.o *.ll *.as
