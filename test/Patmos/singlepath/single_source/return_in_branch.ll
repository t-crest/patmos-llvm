; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 2=1 1=2 3=1
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test that we can return from inside a branch.
; The following is the equivalent C code:
; #include <stdio.h>
; 
; volatile int _1 = 1;
; volatile int _2 = 2;
; 
; int init_func(int x){
; 	if(x == _1){
; 		return _2 * x;
; 	} else {
; 		return _2 - _1;
; 	}
; }
; 
; int main(){
; 	int x;
; 	scanf("%d", &x);
; 	printf("%d\n", init_func(x));
; }
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1
@_2 = global i32 2
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %x) {
entry:
  %0 = load volatile i32* @_1
  %cmp = icmp eq i32 %0, %x
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %1 = load volatile i32* @_2
  %mul = mul nsw i32 %1, %x
  ret i32 %mul

if.else:                                          ; preds = %entry
  %2 = load volatile i32* @_2
  %3 = load volatile i32* @_1
  %sub = sub nsw i32 %2, %3
  ret i32 %sub
}

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

