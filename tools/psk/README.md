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

Benchmark Analysis Demo
-----------------------

(1) compile (=> elf) and generate a pml file [clang]
(2) simulate, analyze trace and generate flow facts for benchmark
(3) export trace flow facts (if necessary) and jumptables to .ais format
(4) run aiT

Trace Analysis
--------------

Events:
 - CALL(f,callsite.top)             if pc == f.blocks.first.address
 - BB(bb)                           if pc == bb.address
 - RETURN(returnsite,callsites.top) if pc == bb.instructions[-1-DELAY_SLOTS].address
                                       && bb.successors.empty?
 - LOOPEXIT(loopstack.pop)          while loop_nest_level[pc] < loopstack.size
 - LOOPENTER(loop)                  if pc == loop.header and loopstack.size == loop.depth - 1
 - LOOPCONT(loop)                   if pc == loop.header and loopstack.size == loop.depth
Internal Actions:
 - callsites.push(callsite) if pc == callsite.address
 - callsite.pop             if pc == (callsite + (DELAY_SLOTS + 1) instructions).address

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
"exit test at end" => loop header bound
"exit test at beginning" => loop backedge bound = loop header bound - 1
