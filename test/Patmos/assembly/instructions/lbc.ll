; RUN: %test_no_runtime_execution

define i32 @main() {
entry:  
  %stack_var_1 = alloca i8, i32 4
  store i8 2, i8* %stack_var_1
  %stack_var_2 = getelementptr inbounds i8* %stack_var_1, i32 1
  store i8 3, i8* %stack_var_2
  
  %asm_result = call { i32, i32 } asm "
		lbc	$0	=	[$2]	
		lbc	$1	=	[$2 + 1]	
		nop
	", "=&r,=r,r"
	(i8* %stack_var_1)
	
  ; Extract results
  %result_1 = extractvalue { i32, i32 } %asm_result, 0
  %result_2 = extractvalue { i32, i32 } %asm_result, 1
  
  ; Check correctness
  %correct_1 = icmp eq i32 %result_1, 2
  %correct_2 = icmp eq i32 %result_2, 3
  %correct = and i1 %correct_1, %correct_2
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}