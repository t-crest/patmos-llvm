; RUN: llc < %s | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that multiplying by 5 can be optimized into a shadd2 instruction.
; I.e. x * 5 = (x*4)+x = (x<<2)+x
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK-LABEL: main:
define i32 @main(i32 %value)  {
entry:
  %0 = mul nsw i32 %value, 5

; CHECK: shadd2 $r{{[0-9]+}} = $r[[REGISTER:[0-9]+]], $r[[REGISTER]]

  ret i32 %0
}

