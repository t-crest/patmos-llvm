PATMOS_CLANG:=patmos-clang
MODULES=callcontext1.elf

all: $(MODULES)

%.elf: ../%.bc
	$(PATMOS_CLANG) $(CFLAGS) -O0 -Wall -o $@ -mserialize=$(@:elf=pml) -mpreemit-bitcode=$(@:=.bc) $^
../%.bc: ../%.c
	$(PATMOS_CLANG) ${CFLAGS} -Wall -emit-llvm -S -o $@ $<
clean:
	rm -f $(MODULES)