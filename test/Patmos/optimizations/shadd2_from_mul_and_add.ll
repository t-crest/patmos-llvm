; RUN: llc < %s | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that multiplying by 4 and then adding a number is optimized into a shadd2 instruction.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK-LABEL: main:
define i32 @main(i32 %value, i32 %add)  {
entry:
  %0 = mul nsw i32 %value, 4
  %1 = add nsw i32 %0, %add

; CHECK: shadd2 

  ret i32 %1
}

