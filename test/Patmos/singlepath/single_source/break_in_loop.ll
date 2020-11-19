; RUN: %test_singlepath_execution -O2 0=10 1=10 6=10 7=17 9=17 13=17
; RUN: %test_singlepath_execution "-O2 -mpatmos-disable-vliw=false" 0=10 1=10 6=10 7=17 9=17 13=17
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test conditionally breaking out of a loop.
;
; The following is the equivalent C code:
;
; volatile int _7 = 7;
;
; int main(int x){
; 	#pragma loopbound min 0 max 5
; 	while(x > 0){
; 		if(x == 7){
; 			break;
; 		}
; 		x--;
; 	}
; 	x += 10;
; 	return x;
; }
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////
@_7 = global i32 7
define i32 @main(i32 %x) {
entry:
  br label %while.cond

while.cond:                                       ; preds = %if.end, %entry
  %x.addr.0 = phi i32 [ %x, %entry ], [ %dec, %if.end ]
  %cmp = icmp sgt i32 %x.addr.0, 0
  br i1 %cmp, label %while.body, label %while.end, !llvm.loop !0

while.body:                                       ; preds = %while.cond
  %0 = load volatile i32* @_7
  %cmp1 = icmp eq i32 %x.addr.0, %0
  br i1 %cmp1, label %while.end, label %if.end

if.end:                                           ; preds = %while.body
  %dec = add nsw i32 %x.addr.0, -1
  br label %while.cond

while.end:                                        ; preds = %while.body, %while.cond
  %add = add nsw i32 %x.addr.0, 10
  ret i32 %add
}

!0 = metadata !{metadata !0, metadata !1}
!1 = metadata !{metadata !"llvm.loop.bound", i32 0, i32 5}
