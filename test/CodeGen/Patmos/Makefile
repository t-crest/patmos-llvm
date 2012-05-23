
BUILDDIR=../../../Debug+Asserts/

ON?=2

MARCH ?= patmos
LLC=$(BUILDDIR)/bin/llc -march=$(MARCH)

CLANG=$(BUILDDIR)/bin/clang
OPT=$(BUILDDIR)/bin/opt

# to override default BUILDDIR
-include config.mk


VIEW= \
  -view-dag-combine1-dags \
  -view-legalize-types-dags \
  -view-legalize-dags \
  -view-dag-combine2-dags \
  -view-dag-combine-lt-dags \
  -view-isel-dags \
  -view-sched-dags \
  -view-sunit-dags

VIEW= \
  -view-legalize-dags \
  -view-isel-dags \
  -view-sched-dags


DEBUG=--debug

.PHONY: all clean
.PRECIOUS: %.ll

all: return.s

%.ll: %.c
	$(CLANG) -O$(ON) -S -emit-llvm -o $@ $<

%.s: %.ll
	$(LLC) -O0 $(DEBUG) $(VIEW) -o $@ $<
	@echo

clean:
	rm -f *.s *.o *.ll
