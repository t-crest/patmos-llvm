; RUN: %p/../assert_singlepath.sh llc %s init_func 0=-2 1=1
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Test that a simple if/else statement generates single-path code.
; The following is the equivalent C code:
; #include <stdio.h>
;
; volatile int _1 = 1;
; volatile int _2 = 2;
; 
; int init_func(int cond){
; 	int x = 0;
; 	
; 	if(cond){
; 		x += _1;
; 	}else{
; 		x -= _2;
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
@_2 = global i32 2
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %cond)  {
entry:
  %tobool = icmp eq i32 %cond, 0
  br i1 %tobool, label %if.else, label %if.then

if.then:                                          ; preds = %entry
  %0 = load volatile i32* @_1
  br label %if.end

if.else:                                          ; preds = %entry
  %1 = load volatile i32* @_2
  %sub = sub nsw i32 0, %1
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %x.0 = phi i32 [ %0, %if.then ], [ %sub, %if.else ]
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

