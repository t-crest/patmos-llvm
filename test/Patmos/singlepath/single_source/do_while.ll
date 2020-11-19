; RUN: %test_singlepath_execution -O2 0=1 1=1 2=2 9=9 10=10
; RUN: %test_singlepath_execution "-O2 -mpatmos-disable-vliw=false" 0=1 1=1 2=2 9=9 10=10
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests a do/while statement.
; The following is the equivalent C code:
;
; volatile int _1 = 1;
; 
; int main(int x){
; 	int y = 0;
; 	#pragma loopbound min 1 max 9
; 	do{
; 		y += _1;
; 	}while(y < x);
; 	return y;
; }
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1

define i32 @main(i32 %x) {
entry:
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %y.0 = phi i32 [ 0, %entry ], [ %add, %do.cond ]
  %0 = load volatile i32* @_1
  %add = add nsw i32 %y.0, %0
  br label %do.cond, !llvm.loop !0

do.cond:                                          ; preds = %do.body
  %cmp = icmp slt i32 %add, %x
  br i1 %cmp, label %do.body, label %do.end

do.end:                                           ; preds = %do.cond
  ret i32 %add
}

!0 = metadata !{metadata !0, metadata !1}
!1 = metadata !{metadata !"llvm.loop.bound", i32 1, i32 9}