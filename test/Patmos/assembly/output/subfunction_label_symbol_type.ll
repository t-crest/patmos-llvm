; RUN: llc < %s -mpatmos-max-subfunction-size=32 | FileCheck %s
; RUN: llc < %s -mpatmos-max-subfunction-size=32 -filetype=obj -o %t -mforce-block-labels;\
; RUN: llvm-objdump %t -d -t | FileCheck %s --check-prefix OBJ
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that assembly and object outputs give subfunction labels the `STT_FUNC` symbol type.
;
; Using the "CHECK" filecheck prefix, we ensure assembly output adds the '@function' attribute to subfunctions.
; Using the "OBJ" filecheck prefix, we ensure that the labels of the subfunctions are local ('l') and functions ('F') in the symbol table.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK: .type main,@function
; CHECK: main:
; OBJ: main:
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
	
; CHECK: .type {{.LBB0_[0-9]}},@function
; OBJ: [[INLINE1:.LBB0_[0-9]]]:
  ; Then 12 bytes
  %1 = call i32 asm "
      .inline_2:
		add		$0	=	$1, 4
		add		$0	=	$0, 5
		add		$0	=	$0, 6	
	", "=r, r"
	(i32 %0)

; CHECK: .type {{.LBB0_[0-9]}},@function
; OBJ: [[INLINE2:.LBB0_[0-9]]]:
  ; Then another 20 bytes
  %2 = call i32 asm "
      .inline_3:
		add		$0	=	$1, 7
		add		$0	=	$0, 8
		add		$0	=	$0, 9	
	", "=r, r"
	(i32 %1)

; CHECK: .type {{.LBB0_[0-9]}},@function
; OBJ: [[INLINE3:.LBB0_[0-9]]]:
  %3 = sub i32 %2, 15045
  ret i32 %3 ; =0
}

; The following checks the type of the labels in the symbol table
; OBJ: l F .text {{([[:xdigit:]]{8})}} [[INLINE1]]
; OBJ: l F .text {{([[:xdigit:]]{8})}} [[INLINE2]]
; OBJ: l F .text {{([[:xdigit:]]{8})}} [[INLINE3]]
; OBJ: g F .text {{([[:xdigit:]]{8})}} main