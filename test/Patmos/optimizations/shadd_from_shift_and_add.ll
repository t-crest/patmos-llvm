; RUN: llc < %s | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that shifting left by 1 and then adding a number is optimized into a shadd instruction.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK-LABEL: main:
define i32 @main(i32 %value, i32 %add)  {
entry:
  %0 = shl nsw i32 %value, 1
  %1 = add nsw i32 %0, %add

; CHECK: shadd

  ret i32 %1
}

