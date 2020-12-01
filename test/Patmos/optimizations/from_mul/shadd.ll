; RUN: llc < %s | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that multiplying by 3 can be optimized into a shadd instruction.
; I.e. x * 3 = (x*2)+x = (x<<1)+x
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK-LABEL: main:
define i32 @main(i32 %value)  {
entry:
  %0 = mul nsw i32 %value, 3

; CHECK: shadd $r{{[0-9]+}} = $r[[REGISTER:[0-9]+]], $r[[REGISTER]]

  ret i32 %0
}

