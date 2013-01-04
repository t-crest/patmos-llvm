CLANG=patmos-clang
BC2ALF=alf-llc -march=alf -alf-standalone -alf-memory-areas=0x000-0xfff
SWEET=sweet

LEVEL=../../..
BINDIR=$(LEVEL)/build/bin
LLC=$(BINDIR)/llc -march=patmos
LD=patmos-ld

%.ll: %.c
	$(CLANG) -O1 -o $@ -emit-llvm -S $^
%.elf: %.ll
	$(CLANG) -o $@ --save-temps $<
%.elf.bc: %.elf
	-touch $@
%.yml: %.elf.bc
	$(LLC) --serialize=$@ -o /dev/null $<
%.pml: %.yml
	$(LEVEL)/tools/psk/psk-merge -o $@ $^
%.alf: %.ll
	$(BC2ALF) -o $@ $<
%.ff: %.alf %.pml
	$(SWEET) -i=$< func=main -ae ffg=ub vola=t pu -f co o=$@
	$(LEVEL)/tools/psk/psk-ff2pml -i $(word 2, $^) -o $(word 2, $^) $@
