
BUILDDIR=../../../Debug+Asserts/

ON?=2
TRIPLE=patmos-unknown-elf
ARCH ?= patmos

GDB ?=
ifeq ($(GDB),1)
  GDB = gdb --args
endif

LLCBIN=$(BUILDDIR)/bin/llc
MCBIN=$(BUILDDIR)/bin/llvm-mc
TBLGENBIN=$(BUILDDIR)/bin/llvm-tblgen

LLC=$(LLCBIN) -march=$(ARCH)
OBJDUMP=$(BUILDDIR)/bin/llvm-objdump
MC=$(MCBIN) -arch=$(ARCH)
TBLGEN=$(TBLGENBIN)

CLANG=$(BUILDDIR)/bin/clang
OPT=$(BUILDDIR)/bin/opt

VIEW= \
  -view-dag-combine1-dags \
  -view-legalize-types-dags \
  -view-legalize-dags \
  -view-dag-combine2-dags \
  -view-dag-combine-lt-dags \
  -view-isel-dags \
  -view-sched-dags \
  -view-sunit-dags


DEBUG=--debug


# to override default BUILDDIR, VIEW, ...
-include config.mk


.PHONY: all clean
.PRECIOUS: %.ll

all: return.s

%.ll: %.c
	$(CLANG) -ccc-host-triple $(TRIPLE) -O$(ON) -S -emit-llvm -o $@ $<

%.s: %.ll $(LLCBIN)
	$(GDB) $(LLC) -O0 $(DEBUG) $(VIEW) -show-mc-encoding -o $@ $<
	./hex2bin -i $@
	@echo

%.o: %.ll $(LLCBIN)
	$(GDB) $(LLC) -O0 -filetype=obj -relocation-model=static -o $@ $<

%.as: %.s $(MCBIN)
	$(GDB) $(MC) -filetype=obj -show-inst-operands -assemble -o $@ $<

%.dis: %.o $(MCBIN)
	$(GDB) $(OBJDUMP) -disassemble $<

%.adis: %.as $(MCBIN)
	$(GDB) $(OBJDUMP) -disassemble $<

Patmos.out: $(TBLGENBIN) ../../../lib/Target/Patmos/*.td
	$(GDB) $(TBLGEN) -I=../../../include -I=../../../lib/Target/Patmos/ ../../../lib/Target/Patmos/Patmos.td > Patmos.out

clean:
	rm -f *.s *.o *.ll *.as *.out
