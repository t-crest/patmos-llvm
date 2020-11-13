; RUN: %test_no_runtime_execution

define i32 @main() {
entry:  
  ; We read from the timer device, which is always present
  ; to test reading from local memory.
  ;
  ; Do 2 reads in immediate succession (without using bundles).
  ; This means the two cycle counts differ by 2, which we check 
  ; for correctness

  %asm_result = call { i32, i32, i32, i32 } asm "
		lwl	$0	=	[$4 + 1]	# Timer clock cycles - low word (0xf0020004)
		lwl	$1	=	[$4]	
		lwl	$2	=	[$4 + 1]	
		lwl	$3	=	[$4]	
		nop
	", "=&r,=&r,=&r,=r,r"
	(i32 4026662912) ; Timer clock cycles - high word (0xf0020000)
	
  ; Extract results
  %cycles_low_1 = extractvalue { i32, i32, i32, i32 } %asm_result, 0
  %cycles_high_1 = extractvalue { i32, i32, i32, i32 } %asm_result, 1
  %cycles_low_2 = extractvalue { i32, i32, i32, i32 } %asm_result, 2
  %cycles_high_2 = extractvalue { i32, i32, i32, i32 } %asm_result, 3
  
  ; Cast to i64
  %cycles_low_1_64 = zext i32 %cycles_low_1 to i64
  %cycles_high_1_64 = zext i32 %cycles_high_1 to i64
  %cycles_low_2_64 = zext i32 %cycles_low_2 to i64
  %cycles_high_2_64 = zext i32 %cycles_high_2 to i64
  
  ; Shift the high values into place
  %cycles_high_1_64_sh = shl i64 %cycles_high_1_64, 32
  %cycles_high_2_64_sh = shl i64 %cycles_high_2_64, 32
  
  ; Merge final i64 cycles counts
  %cycles_1 = add i64 %cycles_low_1_64, %cycles_high_1_64_sh
  %cycles_2 = add i64 %cycles_low_2_64, %cycles_high_2_64_sh
  
  ; Ensure the latter count is exactly 2 more than the former
  %cycles_1_plus_2 = add i64 %cycles_1, 2
  %correct = icmp eq i64 %cycles_2, %cycles_1_plus_2
  
  ; Negate result to ensure 0 is returned on success
  %result_0 = xor i1 %correct, 1 
  
  %result = zext i1 %result_0 to i32
  ret i32 %result
}