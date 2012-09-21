target triple = "patmos-unknown-elf"

; add var + var
define i32 @add_rr(i32 %a, i32 %b) nounwind uwtable {
entry:
  %add = add nsw i32 %a, %b
  ret i32 %add
}

; add var + imm
define i32 @add_ri(i32 %a) nounwind uwtable {
entry:
  %add = add nsw i32 %a, 4095
  ret i32 %add
}

; add imm + var
define i32 @add_ir(i32 %a) nounwind uwtable {
entry:
  %add = add nsw i32 4095, %a
  ret i32 %add
}


; add var + imm
define i32 @add_rl(i32 %a) nounwind uwtable {
entry:
  %add = add nsw i32 %a, 4096
  ret i32 %add
}
