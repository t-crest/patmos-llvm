; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 0=0 1=55 2=700 9=4095 10=19500
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests the code generated for two loops, each in a branch. 
; 
; The following is the equivalent C code:
; 
; #include <stdio.h>
; 
; volatile int _1 = 1;
; volatile int _2 = 2;
; 
; int init_func(int cond){
; 	int x = 0;
; 	if(cond%2){
; 		#pragma loopbound min 0 max 99
; 		for(int i = 0; i<(10*cond); i++){
; 			x += i + _1;
; 		}
; 	}else{
; 		#pragma loopbound min 0 max 199
; 		for(int i = 0; i<(20*cond); i++){
; 			x += i - _2;
; 		}
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
@_3 = global i32 3
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %cond)  {
entry:
  %cond.addr = alloca i32
  %x = alloca i32
  %i = alloca i32
  %i2 = alloca i32
  store i32 %cond, i32* %cond.addr
  store i32 0, i32* %x
  %0 = load i32* %cond.addr
  %rem = srem i32 %0, 2
  %tobool = icmp ne i32 %rem, 0
  br i1 %tobool, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  store i32 0, i32* %i
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %if.then
  call void @llvm.loopbound(i32 0, i32 99)
  %1 = load i32* %i
  %2 = load i32* %cond.addr
  %mul = mul nsw i32 10, %2
  %cmp = icmp slt i32 %1, %mul
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %3 = load i32* %i
  %4 = load volatile i32* @_1
  %add = add nsw i32 %3, %4
  %5 = load i32* %x
  %add1 = add nsw i32 %5, %add
  store i32 %add1, i32* %x
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %6 = load i32* %i
  %inc = add nsw i32 %6, 1
  store i32 %inc, i32* %i
  br label %for.cond

for.end:                                          ; preds = %for.cond
  br label %if.end

if.else:                                          ; preds = %entry
  store i32 0, i32* %i2
  br label %for.cond3

for.cond3:                                        ; preds = %for.inc8, %if.else
  call void @llvm.loopbound(i32 0, i32 199)
  %7 = load i32* %i2
  %8 = load i32* %cond.addr
  %mul4 = mul nsw i32 20, %8
  %cmp5 = icmp slt i32 %7, %mul4
  br i1 %cmp5, label %for.body6, label %for.end10

for.body6:                                        ; preds = %for.cond3
  %9 = load i32* %i2
  %10 = load volatile i32* @_2
  %sub = sub nsw i32 %9, %10
  %11 = load i32* %x
  %add7 = add nsw i32 %11, %sub
  store i32 %add7, i32* %x
  br label %for.inc8

for.inc8:                                         ; preds = %for.body6
  %12 = load i32* %i2
  %inc9 = add nsw i32 %12, 1
  store i32 %inc9, i32* %i2
  br label %for.cond3

for.end10:                                        ; preds = %for.cond3
  br label %if.end

if.end:                                           ; preds = %for.end10, %for.end
  %13 = load i32* %x
  ret i32 %13
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
