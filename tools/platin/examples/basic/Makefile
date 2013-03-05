PATMOS_CLANG:=patmos-clang
MODULES=jumptable.bc indirect_call.bc base64.bc

all: $(MODULES)

%.bc: %.c
	$(PATMOS_CLANG) -Wall -O0 -emit-llvm -S -o $@ $<

clean:
	rm -f $(MODULES)