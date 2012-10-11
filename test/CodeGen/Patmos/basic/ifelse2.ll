target triple = "patmos-unknown-elf"

define i32 @ifelse(i32 %a, i32 %b) nounwind readnone {
entry:
  %cmp = icmp sgt i32 %a, %b
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %sub = sub nsw i32 %a, %b
  ret i32 %sub

if.else:                                          ; preds = %entry
  %sub1 = sub nsw i32 %b, %a
  ret i32 %sub1
}
