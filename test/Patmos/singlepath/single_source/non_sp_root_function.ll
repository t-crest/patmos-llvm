; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 2=4 3=3
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests that sinle-path functions that are not part of the root set are supported 
; and that a false branch still calls functions.
; 
; The following is the equivalent C code:
; 
; #include <stdio.h>
; 
; volatile int _2 = 2;
; 
; int non_root(int x){
; 	return x + _2;
; }
; 
; int init_func(int x){
; 	return x%2? x : non_root(x);
; }
; 
; int main(){
; 	int x;
; 	scanf("%d", &x);
; 	printf("%d\n", init_func(x));
; }
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_2 = global i32 2
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @non_root(i32 %x)  {
entry:
  %0 = load volatile i32* @_2
  %add = add nsw i32 %0, %x
  ret i32 %add
}

define i32 @init_func(i32 %x)  {
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

define i32 @main()  {
entry:
  %x = alloca i32
  %call = call i32 (i8*, ...)* @scanf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32* %x) 
  %0 = load i32* %x
  %call1 = call i32 @init_func(i32 %0)
  %call2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), i32 %call1) 
  ret i32 0
}

declare i32 @scanf(i8*, ...) 

declare i32 @printf(i8*, ...) 
