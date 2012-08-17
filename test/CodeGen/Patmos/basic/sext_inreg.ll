target datalayout = "E-S32-p:32:32:32-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f64:32:64-n32"
target triple = "patmos-unknown-elf"

define hidden i32 @sext_inreg(i32 %i) nounwind {

entry:                                   
  %0 = shl i32 %i, 31
  %sext = ashr exact i32 %0, 31
  ret i32 %sext
}

