; RUN: llc %s -mpatmos-singlepath=main -o %t
; XFAIL: *
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests that omitting loop bounds will result in a failure
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1

define i32 @main(i32 %iteration_count)  {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %x.0 = phi i32 [ 0, %entry ], [ %add1, %for.inc ]
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %cmp = icmp slt i32 %i.0, %iteration_count
  br i1 %cmp, label %for.body, label %for.end     ; Should have loop bounds, but doesn't

for.body:                                         ; preds = %for.cond
  %0 = load volatile i32* @_1
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %add = add nsw i32 %i.0, %0
  %add1 = add nsw i32 %x.0, %add
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 %x.0
}
