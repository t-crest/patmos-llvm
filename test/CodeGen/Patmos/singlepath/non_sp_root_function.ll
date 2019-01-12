; RUN: llc  < %s -mpatmos-singlepath=init_func | FileCheck %s
; RUN: llc  < %s -mpatmos-singlepath=init_func -O0
; RUN: llc  < %s -mpatmos-singlepath=init_func -O1
; RUN: llc  < %s -mpatmos-singlepath=init_func -O2

;//////////////////////////////////////////////////////////////////////////////////////////////////
; Tests core that includes a non-root singlepath function.
;//////////////////////////////////////////////////////////////////////////////////////////////////

target triple = "patmos-unknown-unknown-elf"

define i32 @callee() #0 {
entry:
  ret i32 0
}

define i32 @init_func() #0 {
entry:
  %call = call i32 @callee()
  ret i32 %call
}

; CHECK-LABEL: init_func:
; CHECK:						call	callee_sp_
; CHECK:						pmov	$p7 =  $p0

; CHECK-LABEL: callee_sp_:
; CHECK:						pmov	$p1 =  $p7
; CHECK:			( $p1)		mov		$r1 = $r0