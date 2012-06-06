
; storing immediates will cause LI instructions to be emit.

; li = add r0, imm
define void @store_imm(i32* %loc) nounwind uwtable {
entry:
  store i32  4095, i32* %loc, align 4
  ret void
}

; li = sub r0, -imm
define void @store_nimm(i32* %loc) nounwind uwtable {
entry:
  store i32 -4095, i32* %loc, align 4
  ret void
}

; li = addl r0, i32imm
define void @store_long(i32* %loc) nounwind uwtable {
entry:
  store i32  4096, i32* %loc, align 4
  ret void
}
