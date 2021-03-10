; RUN: SUCCESS=true; \
; RUN: for n in {0..3}; do \
; RUN:  echo "n=$n"; \
; RUN:   llc < %s  -mpatmos-subfunction-align=$n | FileCheck %s \
; RUN:     && echo "  success" || SUCCESS=false; \
; RUN: done; $SUCCESS
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that when we set the subfunction alignment to anything under 4 bytes, a 4 byte alignment
; will be used anyway.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_7 = global i32 7

; CHECK: .align 4
; CHECK: .fstart	main, .Ltmp{{[0-9]}}-main, 4
; CHECK-NEXT: main:
define i32 @main()  {
entry:
  ret i32 0
}
