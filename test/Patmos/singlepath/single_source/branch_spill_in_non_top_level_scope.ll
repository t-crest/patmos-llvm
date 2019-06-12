; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 0=0 1=-19 2=-40 6=-76 7=-70 19=2 20=8
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests the spilling of predicate registers inside a non-top-level-scope. I.e. spilling registers because
; we have too many branches inside a loop.
; Because the if/else nesting depth is 6 inside a loop, spilling at least 1 register is required.
; 
; The following is the equivalent C code:
; 
; #include <stdio.h>
; 
; int init_func(int x){
; 	int result = 0;
; 	#pragma loopbound min 0 max 19
; 	for(int i = 0; i < x; i++){
; 		if(i<6){
; 			if(i<5){
; 				if(i<4){
; 					if(i<3){
; 						if(i<2){
; 							if(i<1){
; 								result += 1;
; 							}else{
; 								result -= 1;
; 							}
; 							result -= 2;
; 						}else{
; 							result += 2;
; 						}
; 						result -=3;
; 					}else{
; 						result += 3;
; 					}
; 					result -=4;
; 				}else{
; 					result += 4;
; 				}
; 				result -=5;
; 			}else{
; 				result += 5;
; 			}
; 			result -=6;
; 		}else{
; 			result += 6;
; 		}
; 	}
; 	return result;
; }
; 
; int main(){
; 	int x;
; 	scanf("%d", &x);
; 	printf("%d\n", init_func(x));
; }
; 
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %x)  {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %result.0 = phi i32 [ 0, %entry ], [ %result.6, %for.inc ]
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  call void @llvm.loopbound(i32 0, i32 19)
  %cmp = icmp slt i32 %i.0, %x
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %cmp1 = icmp slt i32 %i.0, 6
  br i1 %cmp1, label %if.then, label %if.else29

if.then:                                          ; preds = %for.body
  %cmp2 = icmp slt i32 %i.0, 5
  br i1 %cmp2, label %if.then3, label %if.else25

if.then3:                                         ; preds = %if.then
  %cmp4 = icmp slt i32 %i.0, 4
  br i1 %cmp4, label %if.then5, label %if.else21

if.then5:                                         ; preds = %if.then3
  %cmp6 = icmp slt i32 %i.0, 3
  br i1 %cmp6, label %if.then7, label %if.else17

if.then7:                                         ; preds = %if.then5
  %cmp8 = icmp slt i32 %i.0, 2
  br i1 %cmp8, label %if.then9, label %if.else13

if.then9:                                         ; preds = %if.then7
  %cmp10 = icmp slt i32 %i.0, 1
  br i1 %cmp10, label %if.then11, label %if.else

if.then11:                                        ; preds = %if.then9
  %add = add nsw i32 %result.0, 1
  br label %if.end

if.else:                                          ; preds = %if.then9
  %sub = add nsw i32 %result.0, -1
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then11
  %result.1 = phi i32 [ %add, %if.then11 ], [ %sub, %if.else ]
  %sub12 = add nsw i32 %result.1, -2
  br label %if.end15

if.else13:                                        ; preds = %if.then7
  %add14 = add nsw i32 %result.0, 2
  br label %if.end15

if.end15:                                         ; preds = %if.else13, %if.end
  %result.2 = phi i32 [ %sub12, %if.end ], [ %add14, %if.else13 ]
  %sub16 = add nsw i32 %result.2, -3
  br label %if.end19

if.else17:                                        ; preds = %if.then5
  %add18 = add nsw i32 %result.0, 3
  br label %if.end19

if.end19:                                         ; preds = %if.else17, %if.end15
  %result.3 = phi i32 [ %sub16, %if.end15 ], [ %add18, %if.else17 ]
  %sub20 = add nsw i32 %result.3, -4
  br label %if.end23

if.else21:                                        ; preds = %if.then3
  %add22 = add nsw i32 %result.0, 4
  br label %if.end23

if.end23:                                         ; preds = %if.else21, %if.end19
  %result.4 = phi i32 [ %sub20, %if.end19 ], [ %add22, %if.else21 ]
  %sub24 = add nsw i32 %result.4, -5
  br label %if.end27

if.else25:                                        ; preds = %if.then
  %add26 = add nsw i32 %result.0, 5
  br label %if.end27

if.end27:                                         ; preds = %if.else25, %if.end23
  %result.5 = phi i32 [ %sub24, %if.end23 ], [ %add26, %if.else25 ]
  %sub28 = add nsw i32 %result.5, -6
  br label %if.end31

if.else29:                                        ; preds = %for.body
  %add30 = add nsw i32 %result.0, 6
  br label %if.end31

if.end31:                                         ; preds = %if.else29, %if.end27
  %result.6 = phi i32 [ %sub28, %if.end27 ], [ %add30, %if.else29 ]
  br label %for.inc

for.inc:                                          ; preds = %if.end31
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 %result.0
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
