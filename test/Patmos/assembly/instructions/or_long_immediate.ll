; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm "
		or		$0	=	$1, 283436 # Long immediate must use immediate > 4095		
	", "=r,r"
	(i32 56)
  
  ; Check correctness
  %correct = icmp eq i32 %0, 283452
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}
