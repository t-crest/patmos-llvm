; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call { i32, i32 } asm sideeffect "
			cmpeq	$$p1	=	$2, 23
			cmpeq	$$p2	=	$3, 31	# Immediate must be <32
	( $$p1)	li		$0		=	1		# We use predicate 'li' to extract the value
	(!$$p1)	li		$0		=	0		# of a predicate register, to avoid using
	( $$p2)	li		$1		=	1		# 'bcopy', which isn't currently supported 
	(!$$p2)	li		$1		=	0		# by inline assembly
	", "=r,=r,r,r,~{p1},~{p2}"
	(i32 55, i32 31)
  
  ; Extract results
  %expect_false = extractvalue { i32, i32 } %0, 0
  %expect_true = extractvalue { i32, i32 } %0, 1
  
  ; Check correctness
  %expect_false_neg = xor i32 %expect_false, 1 ; Negate the first bit
  %correct = and i32 %expect_false_neg, %expect_true
  
  ; Negate result to ensure 0 is returned on success
  %result = xor i32 %correct, 1 
  
  ret i32 %result
}