; RUN: %test_no_runtime_execution
define i32 @main() {
entry:
  %0 = call i32 asm sideeffect "
			pset	$$p1				# We set p1-4 such that we can read
			pclr	$$p2				# them using 'mfs'
			pset	$$p3
			pset	$$p4
			mfs		$0	= 	 $$s0
	", "=r,~{p1},~{p2},~{p3},~{p4}"
	()
	
  ; Extract only the bits 1-4
  %filtered = and i32 %0, 30 ; 30 in binary: ...011110
  
  ; Check correctness using xor such that 
  ; all matching bits result in 0
  %result = xor i32 %filtered, 26 ; 26 in binary: ...011010
  
  ret i32 %result
}
