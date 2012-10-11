
target triple = "patmos-unknown-elf"

define i32 @sub(i32 %a) nounwind uwtable {
entry:
  %x = sub nsw i32 42, %a
  ret i32 %x
}

define i32 @addn(i32 %a) nounwind uwtable {
entry:
  %x = add nsw i32 %a, -42
  ret i32 %x
}
