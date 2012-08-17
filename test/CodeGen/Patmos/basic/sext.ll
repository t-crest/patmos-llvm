target datalayout = "E-S32-p:32:32:32-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f64:32:64-n32"
target triple = "patmos-unknown-elf"

define hidden i32 @sext(i32 %i) nounwind {
entry:
  %cmp = icmp ne i32 %i, 20
  br i1 %cmp, label %exit, label %next

next:
  %phitmp = icmp ne i32 %i, 320
  br label %exit

exit:
  %retval = phi i1 [ %phitmp, %next ], [ false, %entry ]
  %sub = sext i1 %retval to i32
  ret i32 %sub
}

