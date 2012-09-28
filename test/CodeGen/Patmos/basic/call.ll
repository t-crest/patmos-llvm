; ModuleID = 'call.ll'
target datalayout = "E-S32-p:32:32:32-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f64:32:64-n32"
target triple = "patmos-unknown-elf"

@var = external global i32

define i32 @second(i32 %a) nounwind {
entry:
  %sub = sub nsw i32 0, %a
  ret i32 %sub
}

define i32 @first(i32 %a, i32 %b) nounwind {
entry:
  %add = add nsw i32 %a, %b
  %call = call i32 @second(i32 %add)
  ret i32 %call
}

define i32 @main(i32 %argc, i8** %argv) nounwind {
entry:
  %0 = load i32* @var, align 4
  %call = call i32 @first(i32 %0, i32 3)
  ret i32 0
}
