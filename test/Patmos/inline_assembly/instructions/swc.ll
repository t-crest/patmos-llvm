; RUN: %test_no_runtime_execution

define i32 @main() {
entry:  
  %stack_var_1 = alloca i32, i32 2
  %stack_var_2 = getelementptr inbounds i32* %stack_var_1, i32 1
  
  call void asm "
		swc	[$0]		=	$1	
		swc	[$0 + 1]	=	$2	
		nop
	", "r,r,r"
	(i32* %stack_var_1, i32 24, i32 25)
	
  ; Extract results
  %result_1 = load i32* %stack_var_1
  %result_2 = load i32* %stack_var_2
  
  ; Check correctness
  %correct_1 = icmp eq i32 %result_1, 24
  %correct_2 = icmp eq i32 %result_2, 25
  %correct = and i1 %correct_1, %correct_2
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}