CLANG=patmos-clang
LLVM-AS=patmos-llvm-as

%.bc: %.c
	$(CLANG) -O1 -o - -emit-llvm -S $< | $(LLVM-AS) -o $@
