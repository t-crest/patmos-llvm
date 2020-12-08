; RUN: CHECK_STDOUT=true; llc -help %XFAIL-filecheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests there is an entry in the help flag for `mpatmos-max-subfunction`.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK: -mpatmos-max-subfunction-size=<int>       - Maximum size of subfunctions after function splitting, defaults to the method cache size if set to 0. (default: 1024)