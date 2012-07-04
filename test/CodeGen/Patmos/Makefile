
BUILDDIR=../../../Debug+Asserts/
GOLDDIR=../../../../patmos-gold/gold/
NEWLIBDIR=$(BUILDDIR)/patmos-unknown-elf

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
LLVM_LDBIN=$(BUILDDIR)/bin/llvm-ld
LDBIN=$(GOLDDIR)/ld-new

LLC=$(LLCBIN) -march=$(ARCH)
OBJDUMP=$(BUILDDIR)/bin/llvm-objdump
LLVM_AS=$(BUILDDIR)/bin/llvm-as
LLVM_DIS=$(BUILDDIR)/bin/llvm-dis
LLVM_LD=$(LLVM_LDBIN) -L $(NEWLIBDIR)/lib -internalize-public-api-list=_start -no-script
MC=$(MCBIN) -arch=$(ARCH)
TBLGEN=$(TBLGENBIN)
LD=$(LDBIN)

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

%.ll: %.c $(CLANGBIN)
	$(CLANG) -ccc-host-triple $(TRIPLE) -O$(ON) -v -S -emit-llvm -o $@ $<

%: %.c $(CLANGBIN)
	$(CLANG) -ccc-host-triple $(TRIPLE) -O$(ON) -v -o $@ $<

%.s: %.ll $(LLCBIN)
	$(GDB) $(LLC) -O0 $(DEBUG) $(VIEW) -show-mc-encoding -o $@ $<
	./hex2bin -i $@
	@echo

%.o: %.ll $(LLCBIN)
	$(GDB) $(LLC) -filetype=obj -o $@ $<

%-app.ll: %.ll
	$(LLVM_AS) $<
	$(LLVM_LD) -v -b ${@:.ll=.bc} ${<:.ll=.bc} -lc -lpatmos $(NEWLIBDIR)/lib/crt0.o
	$(LLVM_DIS) ${@:.ll=.bc}

%: %.o $(LD)
	$(LD) -o $@ $<

%.o: %.c

%.o: %.s

%.as: %.s $(MCBIN)
	$(GDB) $(MC) -filetype=obj -show-inst-operands -assemble -o $@ $<

%.dis: % $(MCBIN)
	$(GDB) $(OBJDUMP) -r -t -disassemble $<

Patmos.out: $(TBLGENBIN) ../../../lib/Target/Patmos/*.td
	$(GDB) $(TBLGEN) -I=../../../include -I=../../../lib/Target/Patmos/ ../../../lib/Target/Patmos/Patmos.td > Patmos.out

clean:
	rm -f *.s *.o *.ll *.as *.out *.bc
