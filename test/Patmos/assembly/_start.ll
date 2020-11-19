
declare i32 @main()
define i32 @_start() {  
entry:
  call void asm sideeffect "
		li $$r31 = 2064384	# initialize shadow stack pointer (0x1f8000)
		mts $$ss  = $0		# initialize the stack cache's spill pointer (0x200000)
		mts $$st  = $0		# initialize the stack cache's top pointer (0x200000)
		brcfnd $1
	", "r,r,~{r31},~{ss},~{st}"
	(i32 2097152, i32 ()* @main) 
  ; We will never return from this functions, as we have
  ; branched to the main function, which will handle the return.
  ; We do this to avoid having the compiler mess with registers
  ; as it tries to call main
  unreachable
}