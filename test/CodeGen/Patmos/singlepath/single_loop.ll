; RUN: llc  < %s -mpatmos-singlepath=init_func  | FileCheck %s 
target triple = "patmos-unknown-unknown-elf"

@_1 = global i32 1

define i32 @init_func(i32 %iteration_count) #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %x.0 = phi i32 [ 0, %entry ], [ %add1, %for.inc ]
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  call void @llvm.loopbound(i32 0, i32 100)
  %cmp = icmp slt i32 %i.0, %iteration_count
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %0 = load volatile i32* @_1
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %add = add nsw i32 %i.0, %0
  %add1 = add nsw i32 %x.0, %add
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 %x.0
}

; Function Attrs: nounwind
declare void @llvm.loopbound(i32, i32) #1

; CHECK-LABEL: init_func:
; CHECK:pmov $p2 =  $p0
; CHECK:mov $r5 = $r0
; CHECK:mov $r1 = $r0
; CHECK:li $r4 = _1
; CHECK:li $r2 = -1
; CHECK:li $r26 = 101
; CHECK:sws [0] = $r26
; CHECK:.LBB0_1:
; CHECK:( $p2) add $r2 = $r2, 1
; CHECK:( $p2) cmple $p1 = $r3, $r2
; CHECK:pand $p3 =  $p2, !$p1
; CHECK:( $p3) lwc $r6 = [$r4]
; CHECK:( $p2) pmov $p2 = !$p1
; CHECK:( $p3) add $r6 = $r5, $r6
; CHECK:( $p3) add $r1 = $r1, $r6
; CHECK:sub $r26 = $r26, 1
; CHECK:cmplt $p7 = $r0, $r26
; CHECK:( $p7) br .LBB0_1
; CHECK:sws [0] = $r26
; CHECK:( $p3) add $r5 = $r5, 1