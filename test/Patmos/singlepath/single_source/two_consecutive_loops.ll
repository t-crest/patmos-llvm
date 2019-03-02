; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func "0 0"=0 "0 1"=2 "1 0"=1 "1 1"=3 "3 2"=7 "10 10"=30
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test conditionally breaking out of a loop.
;
; The following is the equivalent C code:
; #include <stdio.h>
; 
; volatile int _1 = 1;
; volatile int _2 = 2;
; 
; int init_func(int x, int y){
; 	int result = 0;
; 	
; 	#pragma loopbound min 0 max 9
; 	for(int i = 0; i<x; i++){
; 		result += _1;
; 	}
; 	
; 	#pragma loopbound min 0 max 9
; 	for(int i = 0; i<y; i++){
; 		result += _2;
; 	}
; 	
; 	return result;
; }
; 
; int main(){
; 	int x, y;
; 	scanf("%d %d", &x, &y);
; 	printf("%d\n", init_func(x, y));
; }
;//////////////////////////////////////////////////////////////////////////////////////////////////
@_1 = global i32 1
@_2 = global i32 2
@.str = private unnamed_addr constant [6 x i8] c"%d %d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %x, i32 %y) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %result.0 = phi i32 [ 0, %entry ], [ %add, %for.inc ]
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  call void @llvm.loopbound(i32 0, i32 9)
  %cmp = icmp slt i32 %i.0, %x
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %0 = load volatile i32* @_1
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %add = add nsw i32 %result.0, %0
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  br label %for.cond2

for.cond2:                                        ; preds = %for.inc6, %for.end
  %result.1 = phi i32 [ %result.0, %for.end ], [ %add5, %for.inc6 ]
  %i1.0 = phi i32 [ 0, %for.end ], [ %inc7, %for.inc6 ]
  call void @llvm.loopbound(i32 0, i32 9)
  %cmp3 = icmp slt i32 %i1.0, %y
  br i1 %cmp3, label %for.body4, label %for.end8

for.body4:                                        ; preds = %for.cond2
  %1 = load volatile i32* @_2
  br label %for.inc6

for.inc6:                                         ; preds = %for.body4
  %add5 = add nsw i32 %result.1, %1
  %inc7 = add nsw i32 %i1.0, 1
  br label %for.cond2

for.end8:                                         ; preds = %for.cond2
  ret i32 %result.1
}

declare void @llvm.loopbound(i32, i32)

define i32 @main() {
entry:
  %x = alloca i32
  %y = alloca i32
  %call = call i32 (i8*, ...)* @scanf(i8* getelementptr inbounds ([6 x i8]* @.str, i32 0, i32 0), i32* %x, i32* %y)
  %0 = load i32* %x
  %1 = load i32* %y
  %call1 = call i32 @init_func(i32 %0, i32 %1)
  %call2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), i32 %call1)
  ret i32 0
}

declare i32 @scanf(i8*, ...)

declare i32 @printf(i8*, ...)
