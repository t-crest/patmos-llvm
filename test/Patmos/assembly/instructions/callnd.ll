; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm sideeffect "
		mfs		$$r9	=	$$srb	# Save return info
		mfs		$$r10	=	$$sro
		li		$$r3	=	32
		callnd	func
		mov		$0		= 	$$r1	# Return call result
		mts		$$srb	=	$$r9	# Restore return info
		mts		$$sro	=	$$r10
	", "=&r,~{r1},~{r3},~{r9},~{r10}"
	()
  
  ; Check correctness
  %correct = icmp eq i32 %0, 256	; 32*8 = 256
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}

define i32 @func(i32 %x) {
entry:
  %result = shl i32 %x, 3 ; multiply by 8
  ret i32 %result
}

