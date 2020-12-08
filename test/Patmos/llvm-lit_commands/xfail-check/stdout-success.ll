; RUN: CHECK_STDOUT=true; ( echo "some specific stdout output" ; false ) %XFAIL-filecheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that if the variabel 'CHECK_STDOUT' is set to 'true', the '%XFAIL-filecheck' succeeds
; if the command fails and outputs the right text to stdout.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK: some specific stdout output