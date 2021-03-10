; RUN: llc < %s -mpatmos-subfunction-align=32 -mpatmos-max-subfunction-size=32 | FileCheck %s
; RUN: llc < %s -mpatmos-subfunction-align=32 -mpatmos-max-subfunction-size=32 -filetype=obj -o %t -mforce-block-labels;\
; RUN: patmos-ld %t -nostdlib -static -o %t --section-start .text=1;\
; RUN: llvm-objdump %t -d | FileCheck %s --check-prefix ALIGN
; RUN: LLC_ARGS="-mpatmos-subfunction-align=32 -mpatmos-max-subfunction-size=32";\
; RUN: PASIM_ARGS="--mcsize=32"; %test_no_runtime_execution
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////

;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK: .align 32
; CHECK: .fstart	main, .Ltmp{{[0-9]}}-main, 32
; CHECK-NEXT: main:

; ALIGN: main:
; ALIGN-NEXT: {{[0-9]*[0|2|4|6|8|a|c|e]4}}:
define i32 @main()  {
entry:
  ; First 24 bytes
  %0 = call i32 asm "
      .inline_1:
		li		$0	=	5001		# Long arithmetic, 8 bytes each
		add		$0	=	$0, 5002
		add		$0	=	$0, 5003
	", "=r"
	()
	
; CHECK: .align 32
; CHECK: .fstart	[[INLINE1:.LBB0_[0-9]]], .Ltmp{{[0-9]}}-[[INLINE1]], 32
; CHECK-LABEL: .inline_1:

; ALIGN: .inline_1:
; ALIGN-NEXT: {{[0-9]*[0|2|4|6|8|a|c|e]4}}:
  ; Then 12 bytes
  %1 = call i32 asm "
      .inline_2:
		add		$0	=	$1, 4
		add		$0	=	$0, 5
		add		$0	=	$0, 6	
	", "=r, r"
	(i32 %0)

; CHECK: .align 32
; CHECK: .fstart	[[INLINE2:.LBB0_[0-9]]], .Ltmp{{[0-9]}}-[[INLINE2]], 32
; CHECK-LABEL: .inline_2:

; ALIGN: .inline_2:
; ALIGN-NEXT: {{[0-9]*[0|2|4|6|8|a|c|e]4}}:
  ; Then another 20 bytes
  %2 = call i32 asm "
      .inline_3:
		add		$0	=	$1, 7
		add		$0	=	$0, 8
		add		$0	=	$0, 9	
	", "=r, r"
	(i32 %1)

; CHECK: .align 32
; CHECK: .fstart	[[INLINE3:.LBB0_[0-9]]], .Ltmp{{[0-9]}}-[[INLINE3]], 32	
; CHECK-LABEL: .inline_3:

; ALIGN: .inline_3:
; ALIGN-NEXT: {{[0-9]*[0|2|4|6|8|a|c|e]4}}:

  %3 = sub i32 %2, 15045
  ret i32 %3 ; =0
}
