

define i32 @abs_ite(i32 %a) nounwind uwtable {
entry:
  %a.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  %0 = load i32* %a.addr, align 4
  %cmp = icmp sge i32 %0, 0
  br i1 %cmp, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  %1 = load i32* %a.addr, align 4
  br label %cond.end

cond.false:                                       ; preds = %entry
  %2 = load i32* %a.addr, align 4
  %sub = sub nsw i32 0, %2
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i32 [ %1, %cond.true ], [ %sub, %cond.false ]
  ret i32 %cond
}



define i32 @abs_select(i32 %a) nounwind uwtable readnone {
entry:
  %cmp = icmp sgt i32 %a, 0
  %sub = sub nsw i32 0, %a
  %cond = select i1 %cmp, i32 %a, i32 %sub
  ret i32 %cond
}

