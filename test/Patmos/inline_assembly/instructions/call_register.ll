; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm sideeffect "
		mfs		$$r9	=	$$srb	# Save return info
		mfs		$$r10	=	$$sro
		li		$$r3	=	20
		call	$1
		add		$$r3	=	$$r3, 1 # Ensure delay slots are used
		add		$$r3	=	$$r3, 2
		add		$$r3	=	$$r3, 1
		mov		$0		= 	$$r1	# Return call result
		mts		$$srb	=	$$r9	# Restore return info
		mts		$$sro	=	$$r10
	", "=&r,r,~{r1},~{r3},~{r9},~{r10}"
	(i32 (i32)* @func)
  
  ; Check correctness
  %correct = icmp eq i32 %0, 48	; (20 + 4)*2 = 48
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}

define i32 @func(i32 %x) {
entry:
  %result = shl i32 %x, 1 ; multiply by 2
  ret i32 %result
}

