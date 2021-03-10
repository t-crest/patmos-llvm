; RUN: llc < %s | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that the default subfunction alignment is 16 bytes.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK: .align 16
; CHECK: .fstart	main, .Ltmp{{[0-9]}}-main, 16
; CHECK-NEXT: main:
define i32 @main(i32 %cond)  {
entry:
  %tobool = icmp eq i32 %cond, 0
  br i1 %tobool, label %if.else, label %if.then

if.then:                                          ; preds = %entry
  %0 = add nsw i32 %cond, 1
  br label %if.end

if.else:                                          ; preds = %entry
  %1 = add nsw i32 %cond, 3
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %result = phi i32 [ %0, %if.then ], [ %1, %if.else ]
  ret i32 %result
}
