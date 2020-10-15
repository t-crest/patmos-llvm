; RUN: %test_no_runtime_execution
; XFAIL: *
define i32 @main() {
entry:
  %0 = call i32 asm sideeffect "
		bcopy	%0		= 	%1, 0, $$p0
	", "=r,r"
	(i32 2)
    
  ; Check correctness
  %correct = icmp eq i32 %0, 3
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}
