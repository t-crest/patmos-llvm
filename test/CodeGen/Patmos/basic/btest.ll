
define i32 @btest4(i32 %flags, i32 %b) nounwind uwtable readnone {
entry:
  %and = and i32 32, %flags
  %cmp = icmp eq i32 %and, 32
  br i1 %cmp, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  %0 = xor i32 %b, 437
  br label %cond.end

cond.false:                                       ; preds = %entry
  %1 = sub i32 %b, 33
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %c = phi i32 [ %0, %cond.true ], [ %1, %cond.false ]
  ret i32 %c

}

