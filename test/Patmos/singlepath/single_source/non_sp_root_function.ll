; RUN: %test_singlepath_execution -O2 2=4 3=3
; RUN: %test_singlepath_execution "-O2 -mpatmos-disable-vliw=false" 2=4 3=3
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests that sinle-path functions that are not part of the root set are supported 
; and that a false branch still calls functions.
; 
; The following is the equivalent C code:
; 
; volatile int _2 = 2;
; 
; int non_root(int x){
; 	return x + _2;
; }
; 
; int main(int x){
; 	return x%2? x : non_root(x);
; }
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_2 = global i32 2

define i32 @non_root(i32 %x)  {
entry:
  %0 = load volatile i32* @_2
  %add = add nsw i32 %0, %x
  ret i32 %add
}

define i32 @main(i32 %x)  {
entry:
  %rem3 = and i32 %x, 1
  %tobool = icmp eq i32 %rem3, 0
  br i1 %tobool, label %cond.false, label %cond.true

cond.true:                                        ; preds = %entry
  br label %cond.end

cond.false:                                       ; preds = %entry
  %call = call i32 @non_root(i32 %x)
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i32 [ %x, %cond.true ], [ %call, %cond.false ]
  ret i32 %cond
}
