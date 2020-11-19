; RUN: %test_singlepath_execution -O2 1=2 4=5 5=3 7=5
; RUN: %test_singlepath_execution "-O2 -mpatmos-disable-vliw=false" 1=2 4=5 5=3 7=5
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test that we can return from inside a branch.
; The following is the equivalent C code:
;
; volatile int _1 = 1;
; volatile int _2 = 2;
; volatile int _5 = 5;
; 
; int main(int x){
; 	if(x < _5){
; 		return x + _1;
; 	} else {
; 		return x - _2;
; 	}
; }
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1
@_2 = global i32 2
@_5 = global i32 5

define i32 @main(i32 %x) {
entry:
  %0 = load volatile i32* @_5
  %cmp = icmp sgt i32 %0, %x
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %1 = load volatile i32* @_1
  %add = add nsw i32 %x, %1
  br label %return

if.else:                                          ; preds = %entry
  %2 = load volatile i32* @_2
  %sub = sub nsw i32 %x, %2
  br label %return

return:                                           ; preds = %if.else, %if.then
  %retval.0 = phi i32 [ %add, %if.then ], [ %sub, %if.else ]
  ret i32 %retval.0
}
