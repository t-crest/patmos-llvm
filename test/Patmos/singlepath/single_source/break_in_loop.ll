; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 0=10 1=10 6=10 7=17 9=17 13=17
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test conditionally breaking out of a loop.
;
; The following is the equivalent C code:
; #include <stdio.h>
; 
; volatile int _7 = 7;
; 
; int init_func(int x){
; 	#pragma loopbound min 0 max 5
; 	while(x > 0){
; 		if(x == _7){
; 			break;
; 		}
; 		x--;
; 	}
; 	x += 10;
; 	return x;
; }
; 
; int main(){
; 	int x;
; 	scanf("%d", &x);
; 	printf("%d\n", init_func(x));
; }
;//////////////////////////////////////////////////////////////////////////////////////////////////
@_7 = global i32 7
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %x) {
entry:
  br label %while.cond

while.cond:                                       ; preds = %if.end, %entry
  %x.addr.0 = phi i32 [ %x, %entry ], [ %dec, %if.end ]
  call void @llvm.loopbound(i32 0, i32 5)
  %cmp = icmp sgt i32 %x.addr.0, 0
  br i1 %cmp, label %while.body, label %while.end

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


