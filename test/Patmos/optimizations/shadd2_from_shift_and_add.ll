; RUN: llc < %s | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that shifting left by 2 and then adding a number is optimized into a shadd2 instruction.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK-LABEL: main:
define i32 @main(i32 %value, i32 %add)  {
entry:
  %0 = shl nsw i32 %value, 2
  %1 = add nsw i32 %0, %add

; CHECK: shadd2

  ret i32 %1
}

