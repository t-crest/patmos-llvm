; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm "
		add		$0	=	$1, $2		
	", "=r,r,r"
	(i32 4, i32 5)
  
  ; Check correctness
  %correct = icmp eq i32 %0, 9
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}
