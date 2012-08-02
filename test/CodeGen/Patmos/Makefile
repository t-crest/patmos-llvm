
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

LLCBIN=$(BUILDDIR)/bin/llc
MCBIN=$(BUILDDIR)/bin/llvm-mc
TBLGENBIN=$(BUILDDIR)/bin/llvm-tblgen
LLVM_LDBIN=$(BUILDDIR)/bin/llvm-ld
LDBIN=$(GOLDDIR)/ld-new
CLANGBIN=$(BUILDDIR)/bin/clang
OPTBIN=$(BUILDDIR)/bin/opt


### compiler commands ###

LLC=$(LLCBIN) -march=$(ARCH)
OBJDUMP=$(BUILDDIR)/bin/llvm-objdump
LLVM_AS=$(BUILDDIR)/bin/llvm-as
LLVM_DIS=$(BUILDDIR)/bin/llvm-dis

LLVM_LD=$(LLVM_LDBIN) -Llibll -L$(NEWLIBDIR)/lib \
                      -internalize-public-api-list=_start \
                      -internalize-public-api-file=$(NEWLIBDIR)/lib/libllsyms.lst \
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
	$(LLVM_LD) -v -b ${@:.ll=.bc} $(NEWLIBDIR)/lib/crt0.o ${<:.ll=.bc} $(NEWLIBDIR)/lib/libcsyms.o -lc -lpatmos $(NEWLIBDIR)/lib/libllsyms.o -lll
	$(LLVM_DIS) ${@:.ll=.bc}

%: %.o $(LDBIN)
	$(LD) -o $@ $<

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
	rm -f *.s *.o *.ll *.as *.out *.bc ex/*.o ex/*.out ex/*.ll ex/*.bc ex/*.s mc/*.as *-app ex/*-app bc/*-app basic/*-app
