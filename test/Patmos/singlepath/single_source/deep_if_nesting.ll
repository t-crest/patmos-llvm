; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 0=-26 1=-27 2=-21 3=-16 6=5 7=14 8=15
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests deep if/else nesting. Because the if/else nesting depth is 7, spilling at least 1 register is required.
; 
; The following is the equivalent C code:
; 
; #include <stdio.h>
;  
; volatile int cond = 0;
;  
; int init_func(){
; 	int x = cond;
; 	if(cond<7){
; 		if(cond<6){
; 			if(cond<5){
; 				if(cond<4){
; 					if(cond<3){
; 						if(cond<2){
; 							if(cond<1){
; 								x += 1;
; 							}else{
; 								x -= 1;
; 							}
; 							x -= 2;
; 						}else{
; 							x += 2;
; 						}
; 						x -=3;
; 					}else{
; 						x += 3;
; 					}
; 					x -=4;
; 				}else{
; 					x += 4;
; 				}
; 				x -=5;
; 			}else{
; 				x += 5;
; 			}
; 			x -=6;
; 		}else{
; 			x += 6;
; 		}
; 		x -=7;
; 	}else{
; 		x += 7;
; 	}
; 	return x;
; }
;  
;  int main(){
;  	scanf("%d", &cond);
;  	printf("%d\n", init_func());
;  }
;//////////////////////////////////////////////////////////////////////////////////////////////////

@cond = global i32 0
@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func()  {
entry:
  %0 = load volatile i32* @cond
  %1 = load volatile i32* @cond
  %cmp = icmp slt i32 %1, 7
  br i1 %cmp, label %if.then, label %if.else34

if.then:                                          ; preds = %entry
  %2 = load volatile i32* @cond
  %cmp1 = icmp slt i32 %2, 6
  br i1 %cmp1, label %if.then2, label %if.else30

if.then2:                                         ; preds = %if.then
  %3 = load volatile i32* @cond
  %cmp3 = icmp slt i32 %3, 5
  br i1 %cmp3, label %if.then4, label %if.else26

if.then4:                                         ; preds = %if.then2
  %4 = load volatile i32* @cond
  %cmp5 = icmp slt i32 %4, 4
  br i1 %cmp5, label %if.then6, label %if.else22

if.then6:                                         ; preds = %if.then4
  %5 = load volatile i32* @cond
  %cmp7 = icmp slt i32 %5, 3
  br i1 %cmp7, label %if.then8, label %if.else18

if.then8:                                         ; preds = %if.then6
  %6 = load volatile i32* @cond
  %cmp9 = icmp slt i32 %6, 2
  br i1 %cmp9, label %if.then10, label %if.else14

if.then10:                                        ; preds = %if.then8
  %7 = load volatile i32* @cond
  %cmp11 = icmp slt i32 %7, 1
  br i1 %cmp11, label %if.then12, label %if.else

if.then12:                                        ; preds = %if.then10
  %add = add nsw i32 %0, 1
  br label %if.end

if.else:                                          ; preds = %if.then10
  %sub = add nsw i32 %0, -1
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then12
  %x.0 = phi i32 [ %add, %if.then12 ], [ %sub, %if.else ]
  %sub13 = add nsw i32 %x.0, -2
  br label %if.end16

if.else14:                                        ; preds = %if.then8
  %add15 = add nsw i32 %0, 2
  br label %if.end16

if.end16:                                         ; preds = %if.else14, %if.end
  %x.1 = phi i32 [ %sub13, %if.end ], [ %add15, %if.else14 ]
  %sub17 = add nsw i32 %x.1, -3
  br label %if.end20

if.else18:                                        ; preds = %if.then6
  %add19 = add nsw i32 %0, 3
  br label %if.end20

if.end20:                                         ; preds = %if.else18, %if.end16
  %x.2 = phi i32 [ %sub17, %if.end16 ], [ %add19, %if.else18 ]
  %sub21 = add nsw i32 %x.2, -4
  br label %if.end24

if.else22:                                        ; preds = %if.then4
  %add23 = add nsw i32 %0, 4
  br label %if.end24

if.end24:                                         ; preds = %if.else22, %if.end20
  %x.3 = phi i32 [ %sub21, %if.end20 ], [ %add23, %if.else22 ]
  %sub25 = add nsw i32 %x.3, -5
  br label %if.end28

if.else26:                                        ; preds = %if.then2
  %add27 = add nsw i32 %0, 5
  br label %if.end28

if.end28:                                         ; preds = %if.else26, %if.end24
  %x.4 = phi i32 [ %sub25, %if.end24 ], [ %add27, %if.else26 ]
  %sub29 = add nsw i32 %x.4, -6
  br label %if.end32

if.else30:                                        ; preds = %if.then
  %add31 = add nsw i32 %0, 6
  br label %if.end32

if.end32:                                         ; preds = %if.else30, %if.end28
  %x.5 = phi i32 [ %sub29, %if.end28 ], [ %add31, %if.else30 ]
  %sub33 = add nsw i32 %x.5, -7
  br label %if.end36

if.else34:                                        ; preds = %entry
  %add35 = add nsw i32 %0, 7
  br label %if.end36

if.end36:                                         ; preds = %if.else34, %if.end32
  %x.6 = phi i32 [ %sub33, %if.end32 ], [ %add35, %if.else34 ]
  ret i32 %x.6
}

define i32 @main()  {
entry:
  %call = call i32 (i8*, ...)* @scanf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32* @cond) 
  %call1 = call i32 @init_func()
  %call2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), i32 %call1) 
  ret i32 0
}

declare i32 @scanf(i8*, ...) 

declare i32 @printf(i8*, ...) 
