define void @return_void() nounwind uwtable {
entry:
  ret void
}

define i32 @return_arg(i32 %a) nounwind uwtable readnone {
entry:
  ret i32 %a
}
