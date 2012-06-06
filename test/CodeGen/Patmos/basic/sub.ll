

define i32 @sub(i32 %a) nounwind uwtable {
entry:
  %x = sub nsw i32 42, %a
  ret i32 %x
}
