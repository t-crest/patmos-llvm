; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm "
					li	$0	= 	0
		.loop:
					cmpeq	$$p1	=	$0, $1
					add		$0		=	$0, 1
					add		$0		=	$0, 1
		(!$$p1)		brnd	.loop
					sub		$0		=	$0, 2	# After looping ends
	", "=&r,r"
	(i32 16)
  
  ; Check correctness
  %correct = icmp eq i32 %0, 16
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}
