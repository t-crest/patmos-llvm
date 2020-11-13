; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call { i32, i32, i32, i32 } asm sideeffect "
			pand	$$p1	=	 $$p0,  $$p0
			pand	$$p2	=	!$$p0,  $$p0
			pand	$$p3	=	 $$p0, !$$p0
			pand	$$p4	=	!$$p0, !$$p0
	( $$p1)	li		$0		=	1		# We use predicate 'li' to extract the value
	(!$$p1)	li		$0		=	0		# of a predicate register, to avoid using
	( $$p2)	li		$1		=	1		# 'bcopy', which isn't currently supported 
	(!$$p2)	li		$1		=	0		# by inline assembly
	( $$p3)	li		$2		=	1		
	(!$$p3)	li		$2		=	0		
	( $$p4)	li		$3		=	1		
	(!$$p4)	li		$3		=	0		
	", "=r,=r,=r,=r,~{p1},~{p2},~{p3},~{p4}"
	()
  
  ; Extract results
  %expect_true = extractvalue { i32, i32, i32, i32 } %0, 0
  %expect_false_0 = extractvalue { i32, i32, i32, i32 } %0, 1
  %expect_false_1 = extractvalue { i32, i32, i32, i32 } %0, 2
  %expect_false_2 = extractvalue { i32, i32, i32, i32 } %0, 3
  
  ; Check correctness
  %expect_false_0_neg = xor i32 %expect_false_0, 1 ; Negate the first bit
  %expect_false_1_neg = xor i32 %expect_false_1, 1 ; Negate the first bit
  %expect_false_2_neg = xor i32 %expect_false_2, 1 ; Negate the first bit
  %correct_0 = and i32 %expect_false_0_neg, %expect_false_1_neg
  %correct_1 = and i32 %correct_0, %expect_false_2_neg
  %correct = and i32 %correct_1, %expect_true
  
  ; Negate result to ensure 0 is returned on success
  %result = xor i32 %correct, 1 
  
  ret i32 %result
}