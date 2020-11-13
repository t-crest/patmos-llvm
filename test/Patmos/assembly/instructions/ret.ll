; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 (i32)* @func(i32 4)
  
  ; Check correctness
  %correct = icmp eq i32 %0, 19	; (4 * 4) + 3 = 19
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}

define i32 @func(i32 %x) {
entry:
  %x_4 = shl i32 %x, 2 ; multiply by 4
  call void asm sideeffect "
		mov		$$r1	=	$0
		ret
		add		$$r1	=	$$r1, 1
		add		$$r1	=	$$r1, 1
		add		$$r1	=	$$r1, 1
		add		$$r1	=	$$r1, 1 # Shouldn't be run
	", "r,~{r1}"
	(i32 %x_4)
  unreachable
}

