; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm "
		shadd	$0	=	$1, 245000 # Immediate must be >4095 to use the long instruction format		
	", "=r,r"
	(i32 2000)
  
  ; Check correctness
  %correct = icmp eq i32 %0, 249000
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}
