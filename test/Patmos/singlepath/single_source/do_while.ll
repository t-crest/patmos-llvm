; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 0=1 1=1 2=2 9=9 10=10
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests a do/while statement.
; The following is the equivalent C code:
; #include <stdio.h>
; 
; volatile int _1 = 1;
; 
; int init_func(int x){
; 	int y = 0;
; 	#pragma loopbound min 1 max 9
; 	do{
; 		y += _1;
; 	}while(y < x);
; 	return y;
; }
; 
; int main(){
; 	int x;
; 	scanf("%d", &x);
; 	printf("%d\n", init_func(x));
; }
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %x) {
entry:
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %y.0 = phi i32 [ 0, %entry ], [ %add, %do.cond ]
  call void @llvm.loopbound(i32 1, i32 9)
  %0 = load volatile i32* @_1
  %add = add nsw i32 %y.0, %0
  br label %do.cond

do.cond:                                          ; preds = %do.body
  %cmp = icmp slt i32 %add, %x
  br i1 %cmp, label %do.body, label %do.end

do.end:                                           ; preds = %do.cond
  ret i32 %add
}

declare void @llvm.loopbound(i32, i32)

define i32 @main() {
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
