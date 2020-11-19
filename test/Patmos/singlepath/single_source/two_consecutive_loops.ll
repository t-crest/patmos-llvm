; RUN: %test_singlepath_execution -O2 0=0 1=0 4=2 10=7 18=11 20=14
; RUN: %test_singlepath_execution "-O2 -mpatmos-disable-vliw=false" 0=0 1=0 4=2 10=7 18=11 20=14
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test two consecutive loops.
;
; The following is the equivalent C code:
; #include <stdio.h>
; 
; volatile int _1 = 1;
; volatile int _2 = 2;
; 
; int main(int input){
; 	int result = 0;
; 	int x = input/2, y = input/10;
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
;//////////////////////////////////////////////////////////////////////////////////////////////////
@_1 = global i32 1
@_2 = global i32 2

define i32 @main(i32 %input) {
entry:
  %0 = alloca i32
  %result = alloca i32
  %x = alloca i32
  %y = alloca i32
  %i = alloca i32
  %i1 = alloca i32
  store i32 %input, i32* %0
  store i32 0, i32* %result
  %1 = load i32* %0
  %2 = sdiv i32 %1, 2
  store i32 %2, i32* %x
  %3 = load i32* %0
  %4 = sdiv i32 %3, 10
  store i32 %4, i32* %y
  store i32 0, i32* %i
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %5 = load i32* %i
  %6 = load i32* %x
  %7 = icmp slt i32 %5, %6
  br i1 %7, label %for.body, label %for.end, !llvm.loop !0

for.body:                                         ; preds = %for.cond
  %8 = load volatile i32* @_1
  %9 = load i32* %result
  %10 = add nsw i32 %9, %8
  store i32 %10, i32* %result
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %11 = load i32* %i
  %12 = add nsw i32 %11, 1
  store i32 %12, i32* %i
  br label %for.cond

for.end:                                          ; preds = %for.cond
  store i32 0, i32* %i1
  br label %for.cond2

for.cond2:                                        ; preds = %for.inc4, %for.end
  %13 = load i32* %i1
  %14 = load i32* %y
  %15 = icmp slt i32 %13, %14
  br i1 %15, label %for.body3, label %for.end5, !llvm.loop !0

for.body3:                                        ; preds = %for.cond2
  %16 = load volatile i32* @_2
  %17 = load i32* %result
  %18 = add nsw i32 %17, %16
  store i32 %18, i32* %result
  br label %for.inc4

for.inc4:                                         ; preds = %for.body3
  %19 = load i32* %i1
  %20 = add nsw i32 %19, 1
  store i32 %20, i32* %i1
  br label %for.cond2

for.end5:                                         ; preds = %for.cond2
  %21 = load i32* %result
  ret i32 %21
}

!0 = metadata !{metadata !0, metadata !1}
!1 = metadata !{metadata !"llvm.loop.bound", i32 0, i32 9}