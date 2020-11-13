; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm "
	{	(!$$p0)	sub		$0	=	$1, $2	
		( $$p0) add		$0	=	$1, $2	}
	", "=r,r,r"
	(i32 10, i32 9)
  
  ; Check correctness
  %correct = icmp eq i32 %0, 19
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}
