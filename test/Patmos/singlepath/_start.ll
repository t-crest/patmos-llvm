
declare i32 @main(i32)
@input = external global i32
define i32 @_start() {  
entry:
  %0 = ptrtoint i32* @input to i32
  call void asm sideeffect "
		li $$r31 = 2064384
		mov $$r3 = $2
		mts $$ss  = $0
		mts $$st  = $0
		brcfnd $1
	", "r,r,r,~{r3},~{r31},~{ss},~{st}"
	(i32 2097152, i32 (i32)* @main, i32 %0)
  unreachable
}