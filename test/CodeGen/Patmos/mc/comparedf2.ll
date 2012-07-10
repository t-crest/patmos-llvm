; ModuleID = 'comparedf2.o'
target datalayout = "E-S32-p:32:32:32-i8:8:8-i16:16:16-i32:32:32-i64:32:64-n32"
target triple = "patmos-unknown-elf"

define internal fastcc i64 @toRep(double %x) nounwind readnone inlinehint {
entry:
  %0 = bitcast double %x to i64
  ret i64 %0
}

define i32 @__unorddf2(double %a, double %b) nounwind readnone {
entry:
  %call = tail call fastcc i64 @toRep(double %a)
  %and = and i64 %call, 9223372036854775807
  %cmp = icmp ugt i64 %and, 9218868437227405312
  br i1 %cmp, label %lor.end, label %lor.rhs

lor.rhs:                                          ; preds = %entry
  %call1 = tail call fastcc i64 @toRep(double %b)
  %and2 = and i64 %call1, 9223372036854775807
  %cmp3 = icmp ugt i64 %and2, 9218868437227405312
  br label %lor.end

lor.end:                                          ; preds = %lor.rhs, %entry
  %0 = phi i1 [ true, %entry ], [ %cmp3, %lor.rhs ]
  %lor.ext = zext i1 %0 to i32
  ret i32 %lor.ext
}

