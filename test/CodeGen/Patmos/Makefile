
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

# Taken from Wolfang's Lemberg Makefile
LIBLLSYMS=__divdf3,__divsf3,__divdi3,__divsi3,__moddi3,__modsi3,__udivdi3,__udivsi3,__umoddi3,__umodsi3,__ashldi3,__ashrdi3,__lshrdi3,__muldi3,memset,memmove,memcpy
LIBLLSYMS_SOFTFLOAT=__adddf3,__subdf3,__muldf3,__addsf3,__subsf3,__mulsf3,__eqdf2,__gedf2,__gtdf2,__ledf2,__ltdf2,__nedf2,__unorddf2,__eqsf2,__gesf2,__gtsf2,__lesf2,__ltsf2,__nesf2,__unordsf2,__floatsidf,__floatunsidf,__fixdfsi,__fixunsdfsi,__floatsisf,__floatunssisf,__fixsfsi,__extendsfdf2,__truncdfsf2

SYMDEFS=--defsym _shadow_stack_base=0x4000000 --defsym _stack_cache_base=0x3000000

LLCBIN=$(BUILDDIR)/bin/llc
MCBIN=$(BUILDDIR)/bin/llvm-mc
TBLGENBIN=$(BUILDDIR)/bin/llvm-tblgen
LLVM_LDBIN=$(BUILDDIR)/bin/llvm-ld
LDBIN=$(GOLDDIR)/ld-new

LLC=$(LLCBIN) -march=$(ARCH)
OBJDUMP=$(BUILDDIR)/bin/llvm-objdump
LLVM_AS=$(BUILDDIR)/bin/llvm-as
LLVM_DIS=$(BUILDDIR)/bin/llvm-dis
LLVM_LD=$(LLVM_LDBIN) -Llibll -L $(NEWLIBDIR)/lib -internalize-public-api-list=_start,$(LIBLLSYMS_SOFTFLOAT),$(LIBLLSYMS) -no-script

MC=$(MCBIN) -arch=$(ARCH)
TBLGEN=$(TBLGENBIN)
LD=$(LDBIN) $(SYMDEFS)

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
	$(GDB) $(LLC) $(DEBUG) $(VIEW) -filetype=obj -o $@ $<

%-app.ll: %.ll
	$(LLVM_AS) $<
	$(LLVM_LD) -v -b ${@:.ll=.bc} ${<:.ll=.bc} $(NEWLIBDIR)/lib/crt0.o -lc -lpatmos $(NEWLIBDIR)/lib/libllsyms.o -lll
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
	$(GDB) $(OBJDUMP) -r -t -disassemble $<

Patmos.out: $(TBLGENBIN) ../../../lib/Target/Patmos/*.td
	$(GDB) $(TBLGEN) -I=../../../include -I=../../../lib/Target/Patmos/ ../../../lib/Target/Patmos/Patmos.td > Patmos.out

clean:
	rm -f *.s *.o *.ll *.as *.out *.bc ex/*.o ex/*.out ex/*.ll ex/*.bc ex/*.s mc/*.as *-app ex/*-app bc/*-app basic/*-app
