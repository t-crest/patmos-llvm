; RUN: llc  < %s -mpatmos-singlepath=init_func  | FileCheck %s
; RUN: llc  < %s -mpatmos-singlepath=init_func -O0
; RUN: llc  < %s -mpatmos-singlepath=init_func -O1
; RUN: llc  < %s -mpatmos-singlepath=init_func -O2

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

; CHECK-LABEL:	init_func:
; CHECK:						pmov	[[P1:\$p[1-7]]] =  $p0
; CHECK:						mov		[[R1:\$r([1-9]|[12][0-9]|3[01])]] = $r0
; CHECK:						mov		[[R2:\$r([1-9]|[12][0-9]|3[01])]] = $r0
; CHECK:						li		[[R3:\$r([1-9]|[12][0-9]|3[01])]] = _1
; CHECK:						li		[[R4:\$r([1-9]|[12][0-9]|3[01])]] = -1
; CHECK:						li		[[R5:\$r([1-9]|[12][0-9]|3[01])]] = 101

; CHECK-LABEL:	.LBB0_1:
; CHECK:			( [[P1]]) 	add		[[R6:\$r([1-9]|[12][0-9]|3[01])]] = [[R4]], 1
; CHECK:			( [[P1]]) 	cmple	[[P2:\$p[1-7]]] = $r3, [[R6]]
; CHECK:						pand	[[P3:\$p[1-7]]] =  [[P1]], ![[P2]]
; CHECK:			( [[P3]]) 	lwc		[[R7:\$r([1-9]|[12][0-9]|3[01])]] = {{\[}}[[R3]]{{\]}}
; CHECK:			( [[P1]]) 	pmov	[[P1]] = ![[P2]]
; CHECK:			( [[P3]]) 	add		[[R8:\$r([1-9]|[12][0-9]|3[01])]] = [[R1]], [[R7]]
; CHECK:			( [[P3]]) 	add		[[R2]] = [[R2]], [[R8]]
; CHECK:						sub		[[R5]] = [[R5]], 1
; CHECK:						cmplt 	[[P4:\$p[1-7]]] = $r0, [[R5]]
; CHECK:			( [[P4]])	br		.LBB0_1
; CHECK:			( [[P3]]) 	add		[[R1]] = [[R1]], 1