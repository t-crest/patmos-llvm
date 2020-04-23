; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func %DEBUG_TYPE %LINK_LIBS 0=1 1=2 2=5 3=10 10=101 123=15130
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests a simple expression which includes a multiply.
; The following is the equivalent C code:
; #include <stdio.h>
; 
; int init_func(int x){
; 	return (x * x) + 1;
; }
; 
; int main(){
; 	int x;
; 	scanf("%d", &x);
; 	printf("%d\n", init_func(x));
; }
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////

@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %x) {
entry:
  %mul = mul nsw i32 %x, %x
  %add = add nsw i32 %mul, 1
  ret i32 %add
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
