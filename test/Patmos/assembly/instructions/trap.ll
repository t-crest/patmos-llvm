; RUN: %test_no_runtime_execution

define i32 @main() {
entry:    
  %0 = call i32 asm sideeffect "
		li		$$r16		=	10			# Set an initial value for r16
		swl		[$1]		=	$2			# Setup interrupt handler
		trap	16							# Trigger interrupt handler
		nop									# Buffer nops
		nop
		nop
		mov		$0			=	$$r16
	", "=r,r,r,~{r16}"
	( i32 4026597568,				; Exception handler register 16 address (0xf00100c0)
	  void ()* @interrupt_handler
	)
  ; Check correctness
  %correct = icmp eq i32 %0, 1283
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}

define void @interrupt_handler() {
entry:
  ; In the interrupt handler, we set the r16 register
  ; so that we can check it in main.
  call void asm "
		li	$$r16 = 1283
		xret
	", "~{r16}"
	()
  unreachable
}
