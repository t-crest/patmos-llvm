; RUN: llc < %s -mpatmos-subfunction-align=8 -mpatmos-basicblock-align=16 \
; RUN: -mpatmos-max-subfunction-size=80 | FileCheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that when both -mpatmos-basicblock-align and -mpatmos-subfunction-align are used,
; -mpatmos-subfunction-align takes precedence for blocks starting a subfunction.
;
; We test this by having the first function be small enough to be in one subfunction,
; which means no blocks start a subfunction, which means they should stick to basicblock-align.
; The second function is too large to be in one subfunction. Therefore it will end up having
; some blocks start subfunctions, which need to stick to subfunction-align, even though
; thats smaller than basicblock-align.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_7 = global i32 7

; CHECK: .align 8
; CHECK: .fstart	main, .Ltmp{{[0-9]}}-main, 8
; CHECK-NEXT: main:
define i32 @main()  {
entry:
  %0 = load volatile i32* @_7
  %tobool = icmp eq i32 %0, 7
  br i1 %tobool, label %if.else, label %if.then

; CHECK: .align 16
; CHECK-NEXT: # %if.else
if.else:                                          ; preds = %entry
  %1 = sub nsw i32 %0, 7
  br label %if.end
 
; CHECK: .align 16
; CHECK-NEXT: # %if.then
if.then:                                          ; preds = %entry
  %2 = add nsw i32 %0, 2
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %result = phi i32 [ %2, %if.then ], [ %1, %if.else ]
  ret i32 %result
}

; CHECK: .align 8
; CHECK: .fstart	main2, .Ltmp{{[0-9]}}-main2, 8
; CHECK-NEXT: main2:
define i32 @main2()  {
entry:
  %0 = load volatile i32* @_7
  %tobool = icmp eq i32 %0, 7
  br i1 %tobool, label %if.else, label %if.then

; CHECK: .align 8
; CHECK: .fstart	[[ELSE_BLOCK:.LBB1_[0-9]]], .Ltmp{{[0-9]}}-[[ELSE_BLOCK]], 8
; CHECK-NEXT: [[ELSE_BLOCK]]:
if.else:                                          ; preds = %entry
  %1 = sub nsw i32 %0, 7
  %2 = load volatile i32* @_7
  %3 = load volatile i32* @_7
  %4 = load volatile i32* @_7
  %5 = load volatile i32* @_7
  %6 = load volatile i32* @_7
  %7 = load volatile i32* @_7
  br label %if.end
 
; CHECK: .align 8
; CHECK: .fstart	[[THEN_BLOCK:.LBB1_[0-9]]], .Ltmp{{[0-9]}}-[[THEN_BLOCK]], 8
; CHECK-NEXT: [[THEN_BLOCK]]:
if.then:                                          ; preds = %entry
  %8 = add nsw i32 %0, 2
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %result = phi i32 [ %8, %if.then ], [ %1, %if.else ]
  ret i32 %result
}