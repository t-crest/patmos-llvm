; RUN: llc  < %s -mpatmos-singlepath=init_func  | FileCheck %s 
; RUN: llc  < %s -mpatmos-singlepath=init_func -O0
; RUN: llc  < %s -mpatmos-singlepath=init_func -O1
; RUN: llc  < %s -mpatmos-singlepath=init_func -O2
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests the spilling of predicate registers inside a scope. I.e. spilling registers because
; we have too many branches
; The following is the equivalent C code:
;
; volatile int _1 = 1;
; volatile int _3 = 3;
; 
; int init_func(int x){
; 	int result = 0;
; 	_Pragma("loopbound min 0 max 15")
; 	while (result < x) {
; 		if(result==4){
; 			result -= _1;
; 		} else if(result==_3){
; 			result += _3;
; 		} else {
; 			result += _1;
; 		}
; 	}
; 	return result;
; }
;
;//////////////////////////////////////////////////////////////////////////////////////////////////
target triple = "patmos-unknown-unknown-elf"

@_1 = global i32 1
@_3 = global i32 3

define i32 @init_func(i32 %x) #0 {
entry:
  br label %while.cond

while.cond:                                       ; preds = %if.end6, %entry
  %result.0 = phi i32 [ 0, %entry ], [ %result.2, %if.end6 ]
  call void @llvm.loopbound(i32 0, i32 15)
  %cmp = icmp slt i32 %result.0, %x
  br i1 %cmp, label %while.body, label %while.end

while.body:                                       ; preds = %while.cond
  %cmp1 = icmp eq i32 %result.0, 4
  br i1 %cmp1, label %if.then, label %if.else

if.then:                                          ; preds = %while.body
  %0 = load volatile i32* @_1
  %sub = sub nsw i32 %result.0, %0
  br label %if.end6

if.else:                                          ; preds = %while.body
  %1 = load volatile i32* @_3
  %cmp2 = icmp eq i32 %result.0, %1
  br i1 %cmp2, label %if.then3, label %if.else4

if.then3:                                         ; preds = %if.else
  br label %if.end

if.else4:                                         ; preds = %if.else
  br label %if.end

if.end:                                           ; preds = %if.else4, %if.then3
  %.pn.in = phi i32* [ @_3, %if.then3 ], [ @_1, %if.else4 ]
  %.pn = load volatile i32* %.pn.in
  %result.1 = add nsw i32 %result.0, %.pn
  br label %if.end6

if.end6:                                          ; preds = %if.end, %if.then
  %result.2 = phi i32 [ %sub, %if.then ], [ %result.1, %if.end ]
  br label %while.cond

while.end:                                        ; preds = %while.cond
  ret i32 %result.0
}

; Function Attrs: nounwind
declare void @llvm.loopbound(i32, i32) #1

; CHECK-LABEL:	init_func:
; CHECK:						pmov	$p2 =  $p0
; CHECK:						mov		$r1 = $r0
; CHECK:						li		$r2 = _1
; CHECK:						li		$r26 = 16
; CHECK:						sws		[0] = $r26
; CHECK-LABEL:	.LBB0_1:
; CHECK:			( $p2)		cmple	$p1 = $r3, $r1
; CHECK:						pand	$p3 =  $p2, !$p1
; CHECK:			( $p2)		pmov	$p2 = !$p1
; CHECK:			( $p3)		cmpneq	$p1 = $r1, 4
; CHECK:						pand	$p4 =  $p3, !$p1
; CHECK:						pand	$p3 =  $p3,  $p1
; CHECK:			( $p3)		li		$r4 = _3
; CHECK:			( $p3)		lwc		$r5 = [$r4]
; CHECK:						sub		$r26 = $r26, 1
; CHECK:			( $p3)		cmpeq	$p1 = $r1, $r5
; CHECK:						pand	$p5 =  $p3, !$p1
; CHECK:			( $p5)		li		$r4 = _1
; CHECK:			( $p3)		lwc		$r4 = [$r4]
; CHECK:						cmplt	$p7 = $r0, $r26
; CHECK:			( $p3)		add		$r1 = $r1, $r4
; CHECK:			( $p4)		lwc		$r4 = [$r2]
; CHECK:			( $p7)		br		.LBB0_1
; CHECK:			( $p4)		sub		$r1 = $r1, $r4
; CHECK:						sws		[0] = $r26















































