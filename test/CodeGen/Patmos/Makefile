
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

SYMDEFS=--defsym _shadow_stack_base=0x4000000 --defsym _stack_cache_base=0x3000000 \
        --defsym __heap_start=end             --defsym __heap_end=0x2000000

### compiler binaries ###

BINARY_PREFIX=patmos-
LLCBIN=$(BUILDDIR)/bin/$(BINARY_PREFIX)llc
MCBIN=$(BUILDDIR)/bin/$(BINARY_PREFIX)llvm-mc
TBLGENBIN=$(BUILDDIR)/bin/$(BINARY_PREFIX)llvm-tblgen
LLVM_LDBIN=$(BUILDDIR)/bin/$(BINARY_PREFIX)llvm-ld
LDBIN=$(GOLDDIR)/$(BINARY_PREFIX)ld-new
CLANGBIN=$(BUILDDIR)/bin/$(BINARY_PREFIX)clang
OPTBIN=$(BUILDDIR)/bin/$(BINARY_PREFIX)opt


### compiler commands ###

LLC=$(LLCBIN) -march=$(ARCH)
OBJDUMP=$(BUILDDIR)/bin/$(BINARY_PREFIX)llvm-objdump
LLVM_AS=$(BUILDDIR)/bin/$(BINARY_PREFIX)llvm-as
LLVM_DIS=$(BUILDDIR)/bin/$(BINARY_PREFIX)llvm-dis

LLVM_LD=$(LLVM_LDBIN) -lrt -L$(NEWLIBDIR)/lib \
                      -internalize-public-api-list=_start \
                      -internalize-public-api-file=$(NEWLIBDIR)/lib/librtsyms.lst \
                      -no-script

MC=$(MCBIN) -arch=$(ARCH)
TBLGEN=$(TBLGENBIN)
LD=$(LDBIN) $(SYMDEFS)

CLANG=$(CLANGBIN)
OPT=$(OPTBIN)

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

ifeq ($(ON),0)
  LLVM_LD += --disable-opt
endif

CLEAN_SUFFIXES=.s .o .ll .as .out .bc .dis


.PHONY: all clean %.clean
.PRECIOUS: %.ll %-app.ll

all: return.s

%.ll: %.c $(CLANGBIN)
	$(CLANG) -ccc-host-triple $(TRIPLE) -O$(ON) -v -S -emit-llvm -o $@ $<

%: %.c $(CLANGBIN)
	$(CLANG) -ccc-host-triple $(TRIPLE) -O$(ON) -v -o $@ $<

%.s: %.ll $(LLCBIN)
	$(GDB) $(LLC) -O$(ON) $(DEBUG) $(VIEW) -show-mc-encoding -o $@ $<
	./hex2bin -i $@
	@echo

%.o: %.ll $(LLCBIN)
	$(GDB) $(LLC) -O$(ON) $(DEBUG) $(VIEW) -filetype=obj -o $@ $<

%-app.ll: %.ll
	$(LLVM_AS) $<
	$(LLVM_LD) -v -b ${@:.ll=.bc} $(NEWLIBDIR)/lib/crt0.o ${<:.ll=.bc} $(NEWLIBDIR)/lib/libcsyms.o -lc -lpatmos $(NEWLIBDIR)/lib/librtsyms.o -lrt
	$(LLVM_DIS) ${@:.ll=.bc}

%: %.o $(LDBIN)
	$(LD) -Ttext 0x100 -o $@ $<

%.bin: %.c $(CLANGBIN)
	$(CLANG) -target $(TRIPLE) -O$(ON) -v -o $@ $<

%.o: %.c

%.o: %.s

%.as: %.s $(MCBIN)
	$(GDB) $(MC) -filetype=obj -assemble -o $@ $<

%.dis: % $(MCBIN)
	$(GDB) $(OBJDUMP) -r -t -disassemble $< > $@

Patmos.out: $(TBLGENBIN) ../../../lib/Target/Patmos/*.td
	$(GDB) $(TBLGEN) -I=../../../include -I=../../../lib/Target/Patmos/ ../../../lib/Target/Patmos/Patmos.td > Patmos.out

%.sim: %-app
	pasim $<

%.clean:
	rm -f $(addprefix $*,$(CLEAN_SUFFIXES))  $*-app $*-app.bc $*-app.ll

clean:
	rm -f *.s *.o *.ll *.as *.out *.bc ex/*.o ex/*.out ex/*.ll ex/*.bc ex/*.s mc/*.as *-app ex/*-app bc/*-app basic/*-app C/*-app C/*.ll C/*.bc C/*.s
