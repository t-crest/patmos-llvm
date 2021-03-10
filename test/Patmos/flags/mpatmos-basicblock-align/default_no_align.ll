; RUN: llc < %s | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that the default basic block alignment is none.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK-LABEL: main:
define i32 @main(i32 %cond)  {
; CHECK-NOT: .align
; CHECK-LABEL: # %entry
entry:
  %tobool = icmp eq i32 %cond, 0
  br i1 %tobool, label %if.else, label %if.then

; CHECK-NOT: .align
; CHECK-LABEL: # %if.then
if.then:                                          ; preds = %entry
  %0 = add nsw i32 %cond, 1
  br label %if.end

; CHECK-NOT: .align
; CHECK-LABEL: # %if.else
if.else:                                          ; preds = %entry
  %1 = add nsw i32 %cond, 3
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %result = phi i32 [ %0, %if.then ], [ %1, %if.else ]
  ret i32 %result
}
