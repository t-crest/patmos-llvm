; RUN: llc < %s -mpatmos-max-subfunction-size=32 %XFAIL-filecheck %s
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that if an inline assembly block is larger than the subfunction size, compilation fails.
; This ensures programs are forced to split assembly functions correctly themselves.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

define i32 @main() {
entry:
  ; We make a block of 64 bytes, which can in no way fit in a 32 byte subfunction
  %0 = call i32 asm "
		li		$0	= 4096	# Long arithmetic, 8 bytes each
		li		$0	= 4096	
		li		$0	= 4096	
		li		$0	= 4096	
		li		$0	= 4096	
		li		$0	= 4096	
		li		$0	= 4096	
		li		$0	= 4096	
	", "=r"
	()
  
  ret i32 %0
}

; CHECK: Inline assembly 
; CHECK: larger than maximum allowed size for subfunctions
