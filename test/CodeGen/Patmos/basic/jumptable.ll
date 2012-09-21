target triple = "patmos-unknown-elf"

define i32 @jumptable(i32 %param) {
entry:
  switch i32 %param, label %bb2 [
    i32 1, label %bb1
    i32 2, label %bb2
    i32 3, label %bb3
    i32 4, label %bb2
  ]
bb1:
  ret i32 1
bb2:
  ret i32 2
bb3:
  ret i32 %param
}
