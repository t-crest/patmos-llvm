; RUN: llc < %s | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that multiplying by 2 and then adding a number is optimized into a shadd instruction.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK-LABEL: main:
define i32 @main(i32 %value, i32 %add)  {
entry:
  %0 = mul nsw i32 %value, 2
  %1 = add nsw i32 %0, %add

; CHECK: shadd 

  ret i32 %1
}

