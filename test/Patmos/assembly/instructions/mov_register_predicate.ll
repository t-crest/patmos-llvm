; RUN: %test_no_runtime_execution
; XFAIL: *
;
; That this doesn't work is a known bug. See github.com/t-crest/patmos-llvm/issues/18
;

define i32 @main() {
entry:
  %0 = call { i32, i32 } asm sideeffect "
		mov		%0		= 	 $$p0
		mov		%1		= 	!$$p0
	", "=r,=r"
	()
  
  ; Extract results
  %expect_true = extractvalue { i32, i32 } %0, 0
  %expect_false = extractvalue { i32, i32 } %0, 1
	
  ; Check correctness
  %expect_false_neg = xor i32 %expect_false, 1 ; Negate the first bit
  %correct = and i32 %expect_false_neg, %expect_true
  
  ; Negate result to ensure 0 is returned on success
  %result = xor i32 %correct, 1 
  
  ret i32 %result
}
