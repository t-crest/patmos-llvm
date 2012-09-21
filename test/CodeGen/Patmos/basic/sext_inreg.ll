target triple = "patmos-unknown-elf"

define hidden i32 @sext_inreg(i32 %i) nounwind {

entry:
  %0 = shl i32 %i, 31
  %sext = ashr exact i32 %0, 31
  ret i32 %sext
}

