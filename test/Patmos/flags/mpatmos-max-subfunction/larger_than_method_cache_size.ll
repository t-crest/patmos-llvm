;      # We reuse the main method by compiling it manually
; RUN: llc %S/matches_method_cache_size.ll -filetype=obj -o %t -mpatmos-max-subfunction-size=64
;      # We then have the ld link it with the rest
; RUN: LD_ARGS="%t"; \ 
;      # We execute with a method cache size smaller than the subfunction size specified before
; RUN: PASIM_ARGS="--mcsize=32"; \ 
; RUN: %test_no_runtime_execution %XFAIL-filecheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that setting the subfunction size to be higher than the method cache size will cause an
; execution error, as the function blocks will be too large for the method cache.
;
; This test is here to indirectly tests that setting the max subfunction size will result in
; blocks of that size. We check the size of the blocks by running the program on the simulator
; with a smaller cache than the block size, meaning it should fail if the blocks are the correct
; size.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK: Method cache size exceeded