; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 0=0 3=3 4=6 6=6 7=8
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests the spilling of predicate registers inside a scope. I.e. spilling registers because
; we have too many branches.
; 
; The following is the equivalent C code:
; 
; #include <stdio.h>
; 
; volatile int _1 = 1;
; volatile int _2 = 2;
; volatile int _3 = 3;
; 
; int init_func(int x){
; 	int result = 0;
; 	_Pragma("loopbound min 0 max 14")
; 	while (result < x) {
; 		if(result==6){
; 			result += _2;
; 		} else if(result==3){
; 			result += _3;
; 		} else {
; 			result += _1;
; 		}
; 	}
; 	return result;
; }
; 
; int main(){
; 	int x;
; 	scanf("%d", &x);
; 	printf("%d\n", init_func(x));
; }
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1
@_2 = global i32 2
@_3 = global i32 3
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %x)  {
entry:
  br label %while.cond

while.cond:                                       ; preds = %if.end7, %entry
  %result.0 = phi i32 [ 0, %entry ], [ %result.2, %if.end7 ]
  call void @llvm.loopbound(i32 0, i32 14)
  %cmp = icmp slt i32 %result.0, %x
  br i1 %cmp, label %while.body, label %while.end

while.body:                                       ; preds = %while.cond
  %cmp1 = icmp eq i32 %result.0, 6
  br i1 %cmp1, label %if.then, label %if.else

if.then:                                          ; preds = %while.body
  br label %if.end7

if.else:                                          ; preds = %while.body
  %cmp2 = icmp eq i32 %result.0, 3
  br i1 %cmp2, label %if.then3, label %if.else5

if.then3:                                         ; preds = %if.else
  br label %if.end

if.else5:                                         ; preds = %if.else
  br label %if.end

if.end:                                           ; preds = %if.else5, %if.then3
  %.pn.in = phi i32* [ @_3, %if.then3 ], [ @_1, %if.else5 ]
  br label %if.end7

if.end7:                                          ; preds = %if.end, %if.then
  %.pn14.in = phi i32* [ @_2, %if.then ], [ %.pn.in, %if.end ]
  %.pn14 = load volatile i32* %.pn14.in
  %result.2 = add nsw i32 %result.0, %.pn14
  br label %while.cond

while.end:                                        ; preds = %while.cond
  ret i32 %result.0
}

declare void @llvm.loopbound(i32, i32) 

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
