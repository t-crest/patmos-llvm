; RUN: %test_no_runtime_execution

define i32 @main() {
entry:  
  %stack_var_1 = alloca i32, i32 3
  %stack_var_2 = getelementptr inbounds i32* %stack_var_1, i32 2
  
  call void asm "
		swm	[$0]		=	$1	
		swm	[$0 + 2]	=	$2	
		nop
	", "r,r,r"
	(i32* %stack_var_1, i32 3744, i32 3844)
	
  ; Extract results
  %result_1 = load i32* %stack_var_1
  %result_2 = load i32* %stack_var_2
  
  ; Check correctness
  %correct_1 = icmp eq i32 %result_1, 3744
  %correct_2 = icmp eq i32 %result_2, 3844
  %correct = and i1 %correct_1, %correct_2
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}