; RUN: llc  < %s -mpatmos-singlepath=init_func  | FileCheck %s 
; RUN: llc  < %s -mpatmos-singlepath=init_func -O0
; RUN: llc  < %s -mpatmos-singlepath=init_func -O1
; RUN: llc  < %s -mpatmos-singlepath=init_func -O2
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test the code generated when branches are so deeply nested spilling registers is required.
; The following is the equivalent C code:
; 
;	volatile int _0 = 0;
;
;	int init_func(){
;		int x = _0;
;		if(_0){
;			if(_0){
;				if(_0){
;					if(_0){
;						if(_0){
;							x += 1;
;						}else{
;							x += 2;
;						}
;					}else{
;						x += 3;
;					}
;				}else{
;					x += 4;
;				}
;			}else{
;				x += 5;
;			}
;		}else{
;			x += 6;
;		}
;		return x;
;	}
; 
; Each 'if' has an associated 'else', which means the condition must be saved in a predicate until
; the else is evaluated after the inner 'if's. Since there are 6 branches, and one function-global
; predicate (for whether the function should run), we need to spill at least 1 register.
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////
target triple = "patmos-unknown-unknown-elf"

@_0 = global i32 0

define i32 @init_func() #0 {

; CHECK-LABEL:	init_func:

entry:
  %0 = load volatile i32* @_0
  %1 = load volatile i32* @_0
  %tobool = icmp eq i32 %1, 0
  br i1 %tobool, label %if.else19, label %if.then

if.then:                                          ; preds = %entry
  %2 = load volatile i32* @_0
  %tobool1 = icmp eq i32 %2, 0
  br i1 %tobool1, label %if.else16, label %if.then2

if.then2:                                         ; preds = %if.then
  %3 = load volatile i32* @_0
  %tobool3 = icmp eq i32 %3, 0
  br i1 %tobool3, label %if.else13, label %if.then4

if.then4:                                         ; preds = %if.then2
  %4 = load volatile i32* @_0
  %tobool5 = icmp eq i32 %4, 0
  br i1 %tobool5, label %if.else10, label %if.then6

if.then6:                                         ; preds = %if.then4
  %5 = load volatile i32* @_0
  %tobool7 = icmp eq i32 %5, 0
  br i1 %tobool7, label %if.else, label %if.then8

if.then8:                                         ; preds = %if.then6
  %add = add nsw i32 %0, 1
  br label %if.end

if.else:                                          ; preds = %if.then6
  %add9 = add nsw i32 %0, 2
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then8
  %x.0 = phi i32 [ %add, %if.then8 ], [ %add9, %if.else ]
  br label %if.end12

if.else10:                                        ; preds = %if.then4
  %add11 = add nsw i32 %0, 3
  br label %if.end12

if.end12:                                         ; preds = %if.else10, %if.end
  %x.1 = phi i32 [ %x.0, %if.end ], [ %add11, %if.else10 ]
  br label %if.end15

if.else13:                                        ; preds = %if.then2
  %add14 = add nsw i32 %0, 4
  br label %if.end15

if.end15:                                         ; preds = %if.else13, %if.end12
  %x.2 = phi i32 [ %x.1, %if.end12 ], [ %add14, %if.else13 ]
  br label %if.end18

if.else16:                                        ; preds = %if.then
  %add17 = add nsw i32 %0, 5
  br label %if.end18

if.end18:                                         ; preds = %if.else16, %if.end15
  %x.3 = phi i32 [ %x.2, %if.end15 ], [ %add17, %if.else16 ]
  br label %if.end21

if.else19:                                        ; preds = %entry
  %add20 = add nsw i32 %0, 6
  br label %if.end21

if.end21:                                         ; preds = %if.else19, %if.end18
  %x.4 = phi i32 [ %x.3, %if.end18 ], [ %add20, %if.else19 ]
  ret i32 %x.4
}

; CHECK:						li		$r2 = _0
; CHECK:						lwc		$r1 = [$r2]
; CHECK:						lwc		$r3 = [$r2]
; CHECK:						cmpeq	$p1 = $r3, 0
; CHECK:						pand	$p2 =  $p0, !$p1
; CHECK:			( $p2) 		lwc		$r2 = [$r2]

; CHECK:						pand	$p3 =  $p0,  $p1
; CHECK:			( $p2)		cmpeq	$p1 = $r2, 0
; CHECK:						pand	$p4 =  $p2,  $p1
; CHECK:						pand	$p2 =  $p2, !$p1
; CHECK:			( $p2)		li		$r2 = _0
; CHECK:			( $p2)		lwc		$r3 = [$r2]
; CHECK:						nop	
; CHECK:			( $p2)		cmpeq	$p1 = $r3, 0
; CHECK:						pand	$p5 =  $p2,  $p1
; CHECK:						pand	$p2 =  $p2, !$p1
; CHECK:			( $p2)		lwc		$r2 = [$r2]
; CHECK:						nop
; CHECK:			( $p2)		cmpeq	$p1 = $r2, 0
; CHECK:						pand	$p6 =  $p2,  $p1
; CHECK:						pand	$p2 =  $p2, !$p1
; CHECK:			( $p2)		li		$r2 = _0
; CHECK:			( $p2)		lwc		$r2 = [$r2]
; CHECK:						nop	
; CHECK:			( $p2)		cmpeq	$p1 = $r2, 0
; CHECK:			( $p2)		bcopy	$r26 = $r26, 0,  $p1
; CHECK:						pand	$p2 =  $p2, !$p1
; CHECK:			( $p2)		add		$r1 = $r1, 1
; CHECK:						btest	$p2 = $r26, 0
; CHECK:			( $p2)		add		$r1 = $r1, 2
; CHECK:			( $p6)		add		$r1 = $r1, 3
; CHECK:			( $p5)		add		$r1 = $r1, 4
; CHECK:			( $p4)		add		$r1 = $r1, 5
; CHECK:						sws		[0] = $r26
; CHECK:			( $p3)		add		$r1 = $r1, 6





















