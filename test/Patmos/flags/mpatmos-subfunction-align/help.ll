; RUN: CHECK_STDOUT=true; llc -help %XFAIL-filecheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests there is an entry in the help flag for `mpatmos-subfunction-align`.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK: -mpatmos-subfunction-align=<uint>         - Alignment for functions and subfunctions (including the preceding size-declaration word) in bytes (default: 16 byte aligned).