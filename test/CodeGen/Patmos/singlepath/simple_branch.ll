; RUN: llc  < %s -mpatmos-singlepath=init_func | FileCheck %s -check-prefix CHECK -check-prefix OPTIM
; RUN: llc  < %s -mpatmos-singlepath=init_func -O0 | FileCheck %s -check-prefix CHECK -check-prefix UNOPT
; RUN: llc  < %s -mpatmos-singlepath=init_func -O1 | FileCheck %s -check-prefix CHECK -check-prefix OPTIM
; RUN: llc  < %s -mpatmos-singlepath=init_func -O2 | FileCheck %s -check-prefix CHECK -check-prefix OPTIM

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

; CHECK-LABEL: init_func:
; CHECK:						li		[[R1:\$r([1-9]|[12][0-9]|3[01])]] = _0
; CHECK:						lwc		[[R2:\$r([1-9]|[12][0-9]|3[01])]] = {{\[}}[[R1]]{{\]}}
; CHECK:						li		[[R3:\$r([1-9]|[12][0-9]|3[01])]] = _1
; CHECK:						lwc		[[R4:\$r([1-9]|[12][0-9]|3[01])]] = {{\[}}[[R3]]{{\]}}
; CHECK:						cmpeq	[[P1:\$p[1-7]]] = [[R4]], 0
; UNOPT:						sws		[[SPL1:\[[0-9]+\]]] = [[R2]] 
; CHECK:						pand	[[P2:\$p[1-7]]] =  $p0, ![[P1]]
; UNOPT:						pand	[[P3:\$p[1-7]]] =  $p0,  [[P1]]
; CHECK:			( [[P2]])	li		[[R5:\$r([1-9]|[12][0-9]|3[01])]] = _2
; CHECK:			( [[P2]])	lwc		[[R6:\$r([1-9]|[12][0-9]|3[01])]] = {{\[}}[[R5]]{{\]}}
; UNOPT:			( [[P2]])	lws		[[R2:\$r([1-9]|[12][0-9]|3[01])]] = [[SPL1]]
; OPTIM:						pand	[[P3:\$p[1-7]]] =  $p0,  [[P1]]
; CHECK:			( [[P2]])	add		$r1 = [[R2]], [[R6]]
; CHECK:			( [[P3]])	li		[[R8:\$r([1-9]|[12][0-9]|3[01])]] = _3
; CHECK:			( [[P3]])	lwc		[[R9:\$r([1-9]|[12][0-9]|3[01])]] = {{\[}}[[R8]]{{\]}}
; UNOPT:			( [[P3]])	lws		[[R2:\$r([1-9]|[12][0-9]|3[01])]] = [[SPL1]]
; CHECK:			( [[P3]])	sub		$r1 = [[R2]], [[R9]]
