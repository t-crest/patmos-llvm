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
 - CALL(f,callsite.top)     if pc == f.blocks.first.address
 - BB(bb)                   if pc == bb.address
 - callsites.push(callsite) if pc == callsite.address
 - RETURN(returnsite,callsites.pop) if blocks.last == return_block

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
