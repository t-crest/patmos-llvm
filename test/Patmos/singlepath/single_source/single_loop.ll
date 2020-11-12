; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func %DEBUG_TYPE %LINK_LIBS 0=0 1=1 2=3 50=1275 100=5050
; RUN: %p/../assert_singlepath.sh llc "-O2 -mpatmos-disable-vliw=false" %s init_func %DEBUG_TYPE %LINK_LIBS 0=0 1=1 2=3 50=1275 100=5050
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests that a bounded loop generates single-path code.
;
; The following is the equivalent C code:
; #include <stdio.h>
; volatile int _1 = 1;
; 
; int init_func(int iteration_count){
; 	int x = 0;
; 	#pragma loopbound min 0 max 99
; 	for(int i = 0; i<iteration_count; i++){
; 		x += i + _1;
; 	}
; 	return x;
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
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %iteration_count)  {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %x.0 = phi i32 [ 0, %entry ], [ %add1, %for.inc ]
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %cmp = icmp slt i32 %i.0, %iteration_count
  br i1 %cmp, label %for.body, label %for.end, !llvm.loop !0

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

!0 = metadata !{metadata !0, metadata !1}
!1 = metadata !{metadata !"llvm.loop.bound", i32 0, i32 99}
