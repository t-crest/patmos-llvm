; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm "
		sub		$0	=	$1, 12834 # Long immediate must use immediate > 4095		
	", "=r,r"
	(i32 15000)
  
  ; Check correctness
  %correct = icmp eq i32 %0, 2166
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}
