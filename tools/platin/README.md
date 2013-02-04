PSK (Patmos Swiss Knife)
========================

This directory contains tools for manipulating, generating and transforming
PML files.

Requirements
------------
$ aptitude install ruby1.9.1-full

Open Questions
--------------
* Do we want to stick with PSK/PML terminology?
  PML is ok, but we try to find a better name for PSK

TODO
----
* Export Labels from LLVM
* Currently, the LLVM machine blocks do not seem to be in order, why?
  This makes it impossible to infer missing labels, but otherwise
  is no problem.

Benchmark Analysis Demo
-----------------------

(1) compile (=> elf) and generate a pml file [clang]
(2) simulate, analyze trace and generate flow facts for benchmark
(3) export trace flow facts (if necessary) and jumptables to .ais format
(4) run aiT

Known Problems:

* aiT infeasibility analysis
mrtc/statemate -O0: aiT reports that the loop body is infeasible, allthough it is
                    executed 3 times in the simulator trace [=> aiT bug?]
mrtc/fft1 -O0:      aiT reports that an infeasible problem, even if no annotations are
	            given [=> aiT bug?]
mrtc/lms -O0:       aiT reports infeasability [=> aiT bug?]:
	            Loop 'main.L1': dead end in first iteration in all contexts

* patmos tool chain problems
mrtc/fac -O0:       Recursing bound missing (=> recursion not supported?)
mrtc/whet -O0:      Needs math libraries (cos,sin) [=> TODO]

* trace analsis takes > 60s
mrtc/adpcm -O0:     120s
mrtc/st -O0:        ...


Trace Analysis
--------------

Events:
 - CALL(f,callsite.top)             if pc == f.blocks.first.address
 - BB(bb)                           if pc == bb.address
 - RETURN(returnsite,callsites.pop) if pc == bb.instructions[-1-DELAY_SLOTS].address
                                       && bb.successors.empty?
 - LOOPEXIT(loopstack.pop)          while loop_nest_level[pc] < loopstack.size
 - LOOPENTER(loop)                  if pc == loop.header and loopstack.size == loop.depth - 1
 - LOOPCONT(loop)                   if pc == loop.header and loopstack.size == loop.depth
Internal Actions:
 - callsites.push(callsite) if pc == callsite.address

TODO: deal with predicated call/returns, prove correct

Recorders:
 - Scope frequency of 'bb' relative to function entry of 'f'
 start: CALL(_,f)
 increment: BB(bb')
 record: RET(f,_)
 - Header Bound for L
 start:    LOOPENTER(L)
 increment:BB(L.header)
 stop:     LOOPEXIT(L)
 - Backedge Bound for L
 start:    LOOPENTER(L)
 increment:LOOPCONT(L)
 record:   LOOPEXIT(L)

Loop Bound Translation Notes
----------------------------
Is this correct ??
"exit test at end" => loop header bound
"exit test at beginning" => loop backedge bound = loop header bound - 1
