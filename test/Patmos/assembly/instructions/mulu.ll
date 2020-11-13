; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call { i32, i32, i32, i32 } asm sideeffect "
		mts		$$sl	= 	$$r0		# Reset sh and sl
		mts		$$sh	= 	$$r0		
		mulu					$4, $5		
	{	mfs		$0		=	$$sl		# Get the value of sl and sh in the delay slot
		mfs		$1		=	$$sh	}	# to ensure they maintain their value
		mfs		$2		=	$$sl		
		mfs		$3		=	$$sh		
	", "=r,=r,=r,=r,r,r,~{sl},~{sh}"
	; We ensure the result of the mult is >2^32, so that the high
	; half of the result is also in play.
	(i32 4, i32 2000000000)
	
  ; Extract results
  %low_before = extractvalue { i32, i32, i32, i32 } %0, 0
  %high_before = extractvalue { i32, i32, i32, i32 } %0, 1
  %low = extractvalue { i32, i32, i32, i32 } %0, 2
  %high = extractvalue { i32, i32, i32, i32 } %0, 3
  
  ; Check correctness
  %low_before_correct = icmp eq i32 %low_before, 0
  %high_before_correct = icmp eq i32 %high_before, 0
  %low_correct = icmp eq i32 %low, 3705032704
  %high_correct = icmp eq i32 %high, 1
  
  ; compile result
  %result_0 = and i1 %low_before_correct, %high_before_correct
  %result_1 = and i1 %result_0, %low_correct
  %result_2 = and i1 %result_1, %high_correct
  
  ; Negate result to ensure 0 is returned on success
  %result_3 = xor i1 %result_2, 1 
  
  %result = zext i1 %result_3 to i32
  ret i32 %result
}
