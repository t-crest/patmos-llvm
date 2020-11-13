; RUN: %test_no_runtime_execution

define i32 @main() {
entry:  
  ; We test writing to local memory by registering
  ; an exception handler with the exception device
  ; and then loading that value.
    
  %0 = call { i32, i32 } asm sideeffect "
		swl	[$2]		=	$3
		swl	[$2 +1 ]	=	$4
		lwl $0			=	[$2]
		lwl $1			=	[$2 + 1]
		nop
	", "=&r,=r,r,r,r"
	( i32 4026597568, ; Exception handler register 16 address (0xf00100c0)
	  i32 24778,
	  i32 24978 
	) 
	
  ; Extract results
  %loaded_1 = extractvalue { i32, i32 } %0, 0
  %loaded_2 = extractvalue { i32, i32 } %0, 1
	  
  ; Check correctness
  %correct_1 = icmp eq i32 %loaded_1, 24778
  %correct_2 = icmp eq i32 %loaded_2, 24978
  %correct = and i1 %correct_1, %correct_2
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}
