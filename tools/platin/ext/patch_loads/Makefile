# rely on implicit rule of make to build the binary
#
LDLIBS = -lelf

.PHONY: all clean

all: patch_loads

patch_loads: patch_loads.c

clean:
	rm -f patch_loads
