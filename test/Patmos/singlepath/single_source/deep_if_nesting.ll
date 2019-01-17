; RUN: %p/../assert_singlepath.sh llc %s init_func 0=1 1=2 2=4 4=8 5=10 6=12 7=13
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests deep if/else nesting. Because of this, spilling at least 1 register is required.
; This test is designed to test this spilling.
; 
; The following is the equivalent C code:
; 
; #include <stdio.h>
; 
; volatile int cond = 0;
; 
; int init_func(){
; 	int x = cond;
; 	if(cond<6){
; 		if(cond<5){
; 			if(cond<4){
; 				if(cond<3){
; 					if(cond<2){
; 						x += 1;
; 					}else{
; 						x += 2;
; 					}
; 				}else{
; 					x += 3;
; 				}
; 			}else{
; 				x += 4;
; 			}
; 		}else{
; 			x += 5;
; 		}
; 	}else{
; 		x += 6;
; 	}
; 	return x;
; }
; 
; int main(){
; 	scanf("%d", &cond);
; 	printf("%d\n", init_func());
; }
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////

@cond = global i32 0
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(){
entry:
  %0 = load volatile i32* @cond
  %1 = load volatile i32* @cond
  %cmp = icmp slt i32 %1, 6
  br i1 %cmp, label %if.then, label %if.else19

if.then:                                          ; preds = %entry
  %2 = load volatile i32* @cond
  %cmp1 = icmp slt i32 %2, 5
  br i1 %cmp1, label %if.then2, label %if.else16

if.then2:                                         ; preds = %if.then
  %3 = load volatile i32* @cond
  %cmp3 = icmp slt i32 %3, 4
  br i1 %cmp3, label %if.then4, label %if.else13

if.then4:                                         ; preds = %if.then2
  %4 = load volatile i32* @cond
  %cmp5 = icmp slt i32 %4, 3
  br i1 %cmp5, label %if.then6, label %if.else10

if.then6:                                         ; preds = %if.then4
  %5 = load volatile i32* @cond
  %cmp7 = icmp slt i32 %5, 2
  br i1 %cmp7, label %if.then8, label %if.else

if.then8:                                         ; preds = %if.then6
  %add = add nsw i32 %0, 1
  br label %if.end

if.else:                                          ; preds = %if.then6
  %add9 = add nsw i32 %0, 2
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then8
  %x.0 = phi i32 [ %add, %if.then8 ], [ %add9, %if.else ]
  br label %if.end12

if.else10:                                        ; preds = %if.then4
  %add11 = add nsw i32 %0, 3
  br label %if.end12

if.end12:                                         ; preds = %if.else10, %if.end
  %x.1 = phi i32 [ %x.0, %if.end ], [ %add11, %if.else10 ]
  br label %if.end15

if.else13:                                        ; preds = %if.then2
  %add14 = add nsw i32 %0, 4
  br label %if.end15

if.end15:                                         ; preds = %if.else13, %if.end12
  %x.2 = phi i32 [ %x.1, %if.end12 ], [ %add14, %if.else13 ]
  br label %if.end18

if.else16:                                        ; preds = %if.then
  %add17 = add nsw i32 %0, 5
  br label %if.end18

if.end18:                                         ; preds = %if.else16, %if.end15
  %x.3 = phi i32 [ %x.2, %if.end15 ], [ %add17, %if.else16 ]
  br label %if.end21

if.else19:                                        ; preds = %entry
  %add20 = add nsw i32 %0, 6
  br label %if.end21

if.end21:                                         ; preds = %if.else19, %if.end18
  %x.4 = phi i32 [ %x.3, %if.end18 ], [ %add20, %if.else19 ]
  ret i32 %x.4
}

define i32 @main(){
entry:
  %call = call i32 (i8*, ...)* @scanf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32* @cond) 
  %call1 = call i32 @init_func()
  %call2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), i32 %call1) 
  ret i32 0
}

declare i32 @scanf(i8*, ...) 

declare i32 @printf(i8*, ...) 
