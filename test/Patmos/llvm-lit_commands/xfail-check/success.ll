; RUN: ( echo "some specific stderr output" 1>&2 ; false ) %XFAIL-filecheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests the '%XFAIL-filecheck' command succeeds of command fails and outputs the right text 
; to stderr
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK: some specific stderr output