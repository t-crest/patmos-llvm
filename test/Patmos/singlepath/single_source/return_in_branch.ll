; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func %DEBUG_TYPE %LINK_LIBS 1=-1 4=2 5=6 7=8
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test that we can return from inside a branch.
; The following is the equivalent C code:
; #include <stdio.h>
; 
; volatile int _1 = 1;
; volatile int _2 = 2;
; volatile int _5 = 5;
; 
; int init_func(int x){
; 	if(x < _5){
; 		return x - _2;
; 	} else {
; 		return x + _1;
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
@_5 = global i32 5
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %x) {
entry:
  %0 = load volatile i32* @_5
  %cmp = icmp sgt i32 %0, %x
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %1 = load volatile i32* @_2
  %sub = sub nsw i32 %x, %1
  br label %return

if.else:                                          ; preds = %entry
  %2 = load volatile i32* @_1
  %add = add nsw i32 %2, %x
  br label %return

return:                                           ; preds = %if.else, %if.then
  %retval.0 = phi i32 [ %sub, %if.then ], [ %add, %if.else ]
  ret i32 %retval.0
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
