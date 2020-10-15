; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call { i32, i32, i32, i32 } asm sideeffect "
			pset	$$p1				# We set p1-4 such that we can flip
			pclr	$$p2				# them using 'mts'
			pset	$$p3
			pclr	$$p4
			mts		$$s0	= 	 $0
	( $$p1)	li		$0		=	1		# We use predicate 'li' to extract the value
	(!$$p1)	li		$0		=	0		# of a predicate register, to avoid using
	( $$p2)	li		$1		=	1		# 'bcopy', which isn't currently supported 
	(!$$p2)	li		$1		=	0		# by inline assembly
	( $$p3)	li		$2		=	1		
	(!$$p3)	li		$2		=	0		
	( $$p4)	li		$3		=	1		 
	(!$$p4)	li		$4		=	0		
	", "=r,=r,=r,=r,r,~{s0}"
	(i32 20) ; Binary: 1 0100, which result in flipping p1-4
	
  ; Extract results
  %expect_false_0 = extractvalue { i32, i32, i32, i32 } %0, 0
  %expect_true_0 = extractvalue { i32, i32, i32, i32 } %0, 1
  %expect_false_1 = extractvalue { i32, i32, i32, i32 } %0, 2
  %expect_true_1 = extractvalue { i32, i32, i32, i32 } %0, 3
  
  ; Check correctness
  %expect_false_0_neg = xor i32 %expect_false_0, 1 ; Negate the first bit
  %expect_false_1_neg = xor i32 %expect_false_1, 1 ; Negate the first bit
  %correct_0 = and i32 %expect_false_0_neg, %expect_false_1_neg
  %correct_1 = and i32 %correct_0, %expect_true_0
  %correct = and i32 %correct_1, %expect_true_1
  
  ; Negate result to ensure 0 is returned on success
  %result = xor i32 %correct, 1 
  
  ret i32 %result
}
