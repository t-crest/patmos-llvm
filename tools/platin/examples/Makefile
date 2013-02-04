CLANG=patmos-clang
LLVM-AS=patmos-llvm-as

%.bc: %.c
	$(CLANG) $(CLANG_FLAGS) -o - -emit-llvm -S $< | $(LLVM-AS) -o $@
