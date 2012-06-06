

define i32 @select_gz(i32 %a, i32 %b, i32 %c) nounwind uwtable {
entry:
  %cond = icmp sgt i32 %c, 0
  %sel = select i1 %cond, i32 %a, i32 %b
  ret i32 %sel
}

define i32 @select_min(i32 %a, i32 %b) nounwind uwtable {
entry:
  %cond = icmp sle i32 %a, %b
  %min = select i1 %cond, i32 %a, i32 %b
  ret i32 %min
}

define i32 @select_max(i32 %a, i32 %b) nounwind uwtable {
entry:
  %cond = icmp sge i32 %a, %b
  %max = select i1 %cond, i32 %a, i32 %b
  ret i32 %max
}

