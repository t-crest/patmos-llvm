; RUN: patmos-llc  < %s -mpatmos-singlepath=init_func  | FileCheck %s 

target triple = "patmos-unknown-unknown-elf"

@_0 = global i32 0
@_1 = global i32 1
@_2 = global i32 2
@_3 = global i32 3

define i32 @init_func() #0 {
entry:
  %0 = load volatile i32* @_0
  %1 = load volatile i32* @_1
  %2 = icmp eq i32 %1, 0
  br i1 %2, label %if.else, label %if.then

if.then:                                          ; preds = %entry
  %3 = load volatile i32* @_2
  %4 = add nsw i32 %0, %3
  br label %if.end

if.else:                                          ; preds = %entry
  %5 = load volatile i32* @_3
  %6 = sub nsw i32 %0, %5
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %x.0 = phi i32 [ %4, %if.then ], [ %6, %if.else ]
  ret i32 %x.0
}

; CHECK-LABEL:	init_func:
; CHECK: 						li		$r1 = _0
; CHECK: 						lwc		$r1 = [$r1]
; CHECK: 						li		$r2 = _1
; CHECK: 						lwc		$r2 = [$r2]
; CHECK: 						nop	
; CHECK: 						cmpeq	$p1 = $r2, 0
; CHECK: 						pand	$p2 =  $p0, !$p1
; CHECK: 						pand	$p3 =  $p0,  $p1
; CHECK: 			( $p2)		li		$r2 = _2
; CHECK: 			( $p3)		li		$r2 = _3
; CHECK: 			( $p2)		lwc		$r2 = [$r2]
; CHECK: 			( $p3) 		lwc		$r2 = [$r2]
; CHECK: 			( $p2)		add		$r1 = $r1, $r2
; CHECK: 			( $p3)		sub		$r1 = $r1, $r2
