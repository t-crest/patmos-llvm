; RUN: llc  < %s -mpatmos-singlepath=init_func  | FileCheck %s 

target triple = "patmos-unknown-unknown-elf"

@_0 = global i32 0, align 4
@_1 = global i32 1, align 4
@_2 = global i32 2, align 4
@_3 = global i32 3, align 4

; Function Attrs: nounwind
define i32 @init_func() #0 {
entry:
  %0 = load volatile i32* @_0
  %1 = load volatile i32* @_1
  %2 = icmp eq i32 %1, 0
  br i1 %2, label %if.else, label %if.then

if.then:                                          ; preds = %entry
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %if.then
  %x.0 = phi i32 [ %0, %if.then ], [ %6, %for.inc ]
  %i.0 = phi i32 [ 0, %if.then ], [ %7, %for.inc ]
  %3 = icmp slt i32 %i.0, 100
  br i1 %3, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %4 = load volatile i32* @_2
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %5 = add nsw i32 %i.0, %4
  %6 = add nsw i32 %x.0, %5
  %7 = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  br label %if.end

if.else:                                          ; preds = %entry
  br label %for.cond2

for.cond2:                                        ; preds = %for.inc4, %if.else
  %x.1 = phi i32 [ %0, %if.else ], [ %11, %for.inc4 ]
  %i1.0 = phi i32 [ 0, %if.else ], [ %12, %for.inc4 ]
  %8 = icmp slt i32 %i1.0, 300
  br i1 %8, label %for.body3, label %for.end5

for.body3:                                        ; preds = %for.cond2
  %9 = load volatile i32* @_3
  br label %for.inc4

for.inc4:                                         ; preds = %for.body3
  %10 = sub nsw i32 %i1.0, %9
  %11 = add nsw i32 %x.1, %10
  %12 = add nsw i32 %i1.0, 1
  br label %for.cond2

for.end5:                                         ; preds = %for.cond2
  br label %if.end

if.end:                                           ; preds = %for.end5, %for.end
  %x.2 = phi i32 [ %x.0, %for.end ], [ %x.1, %for.end5 ]
  ret i32 %x.2
}

; CHECK-LABEL:	init_func:
; CHECK:						li		$r1 = _0
; CHECK:						lwc		$r1 = [$r1]
; CHECK:						li		$r3 = _1
; CHECK:						lwc		$r3 = [$r3]
; CHECK:						li		$r2 = -1
; CHECK:						cmpeq	$p1 = $r3, 0
; CHECK:						pand	$p3 =  $p0,  $p1
; CHECK:						pand	$p2 =  $p0, !$p1
; CHECK:			( $p2)		li		$r3 = 99
; CHECK:			( $p2)		mov		$r5 = $r0
; CHECK:			( $p2)		li		$r4 = _2
; CHECK:						pmov	$p4 =  $p2
; CHECK:.LBB0_1:
; CHECK:			( $p4)		add		$r2 = $r2, 1
; CHECK:			( $p4)		cmplt	$p1 = $r3, $r2
; CHECK:						pand	$p5 =  $p4, !$p1
; CHECK:			( $p5)		lwc		$r6 = [$r4]
; CHECK:			( $p4)		pmov	$p4 = !$p1
; CHECK:			( $p5)		add		$r6 = $r5, $r6
; CHECK:			( $p4)		br		.LBB0_1
; CHECK:			( $p5)		add		$r1 = $r1, $r6
; CHECK:			( $p5)		add		$r5 = $r5, 1
; CHECK:# BB#2:
; CHECK:			( $p3)		mov		$r5 = $r0
; CHECK:			( $p3)		li		$r4 = _3
; CHECK:			( $p3)		li		$r3 = 299
; CHECK:						pmov	$p4 =  $p3
; CHECK:.LBB0_3:
; CHECK:			( $p4)		add		$r2 = $r2, 1
; CHECK:			( $p4)		cmplt	$p1 = $r3, $r2
; CHECK:						pand	$p5 =  $p4, !$p1
; CHECK:			( $p5)		lwc		$r6 = [$r4]
; CHECK:			( $p4)		pmov	$p4 = !$p1
; CHECK:			( $p5)		sub		$r6 = $r5, $r6
; CHECK:			( $p4)		br		.LBB0_3
; CHECK:			( $p5)		add		$r1 = $r1, $r6
; CHECK:			( $p5)		add		$r5 = $r5, 1