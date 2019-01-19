; RUN: %p/../assert_singlepath.sh llc -O2 %s init_func 0=0  1=-19 4=5788 5=-14295
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests the spilling of the predicate register when entering new loop scopes.
; 
; The following is the equivalent C code:
; 
; #include <stdio.h>
; 
; int init_func(int y){
; 	int x = 0;
; 	#pragma loopbound min 0 max 4
; 	for(int l1 = 0; l1<y; l1++){
; 		int g1 = y%2;
; 		#pragma loopbound min 0 max 9
; 		for(int l2 = 0; l2<(2*y); l2++){
; 			int g2 = (y+1)%2;
; 			#pragma loopbound min 0 max 14
; 			for(int l3 = 0; l3<(3*y); l3++){
; 				int g3 = (y+2)%2;
; 				#pragma loopbound min 0 max 19
; 				for(int l4 = 0; l4<(4*y); l4++){
; 					if((y+3)%2) {x++;}else{x--;};
; 				}
; 				if(g3) {x++;}else{x--;};
; 			}
; 			if(g2) {x++;}else{x--;};
; 		}
; 		if(g1) {x++;}else{x--;};
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
; We use 4 nested loops, with each loop calculating a predicate, before the inner loop starts ,
; that must be used after the inner loop ends. Each loop therefore needs 2 predicates to be 
; maintained through the execution of the inner loop. Therefore, since 2*4=8 and we only have
; 6 predicate registers, at least 1 spill of the predicate registers is required.
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////

@.str = private unnamed_addr constant [3 x i8] c"%d\00"
@.str1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @init_func(i32 %y)  {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc42, %entry
  %l1.0 = phi i32 [ 0, %entry ], [ %inc43, %for.inc42 ]
  %x.0 = phi i32 [ 0, %entry ], [ %x.7, %for.inc42 ]
  call void @llvm.loopbound(i32 0, i32 4)
  %cmp = icmp slt i32 %l1.0, %y
  br i1 %cmp, label %for.body, label %for.end44

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc33, %for.body
  %l2.0 = phi i32 [ 0, %for.body ], [ %inc34, %for.inc33 ]
  %x.1 = phi i32 [ %x.0, %for.body ], [ %x.6, %for.inc33 ]
  call void @llvm.loopbound(i32 0, i32 9)
  %mul = shl nsw i32 %y, 1
  %cmp2 = icmp slt i32 %l2.0, %mul
  br i1 %cmp2, label %for.body3, label %for.end35

for.body3:                                        ; preds = %for.cond1
  br label %for.cond5

for.cond5:                                        ; preds = %for.inc24, %for.body3
  %l3.0 = phi i32 [ 0, %for.body3 ], [ %inc25, %for.inc24 ]
  %x.2 = phi i32 [ %x.1, %for.body3 ], [ %x.5, %for.inc24 ]
  call void @llvm.loopbound(i32 0, i32 14)
  %mul6 = mul nsw i32 %y, 3
  %cmp7 = icmp slt i32 %l3.0, %mul6
  br i1 %cmp7, label %for.body8, label %for.end26

for.body8:                                        ; preds = %for.cond5
  br label %for.cond11

for.cond11:                                       ; preds = %for.inc, %for.body8
  %x.3 = phi i32 [ %x.2, %for.body8 ], [ %x.4, %for.inc ]
  %l4.0 = phi i32 [ 0, %for.body8 ], [ %inc17, %for.inc ]
  call void @llvm.loopbound(i32 0, i32 19)
  %mul12 = shl nsw i32 %y, 2
  %cmp13 = icmp slt i32 %l4.0, %mul12
  br i1 %cmp13, label %for.body14, label %for.end

for.body14:                                       ; preds = %for.cond11
  %add15 = and i32 %y, 1
  %tobool = icmp eq i32 %add15, 0
  br i1 %tobool, label %if.then, label %if.else

if.then:                                          ; preds = %for.body14
  %inc = add nsw i32 %x.3, 1
  br label %if.end

if.else:                                          ; preds = %for.body14
  %dec = add nsw i32 %x.3, -1
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %x.4 = phi i32 [ %inc, %if.then ], [ %dec, %if.else ]
  br label %for.inc

for.inc:                                          ; preds = %if.end
  %inc17 = add nsw i32 %l4.0, 1
  br label %for.cond11

for.end:                                          ; preds = %for.cond11
  %rem1066 = and i32 %y, 1
  %tobool18 = icmp eq i32 %rem1066, 0
  br i1 %tobool18, label %if.else21, label %if.then19

if.then19:                                        ; preds = %for.end
  %inc20 = add nsw i32 %x.3, 1
  br label %if.end23

if.else21:                                        ; preds = %for.end
  %dec22 = add nsw i32 %x.3, -1
  br label %if.end23

if.end23:                                         ; preds = %if.else21, %if.then19
  %x.5 = phi i32 [ %inc20, %if.then19 ], [ %dec22, %if.else21 ]
  br label %for.inc24

for.inc24:                                        ; preds = %if.end23
  %inc25 = add nsw i32 %l3.0, 1
  br label %for.cond5

for.end26:                                        ; preds = %for.cond5
  %add = and i32 %y, 1
  %tobool27 = icmp eq i32 %add, 0
  br i1 %tobool27, label %if.then28, label %if.else30

if.then28:                                        ; preds = %for.end26
  %inc29 = add nsw i32 %x.2, 1
  br label %if.end32

if.else30:                                        ; preds = %for.end26
  %dec31 = add nsw i32 %x.2, -1
  br label %if.end32

if.end32:                                         ; preds = %if.else30, %if.then28
  %x.6 = phi i32 [ %inc29, %if.then28 ], [ %dec31, %if.else30 ]
  br label %for.inc33

for.inc33:                                        ; preds = %if.end32
  %inc34 = add nsw i32 %l2.0, 1
  br label %for.cond1

for.end35:                                        ; preds = %for.cond1
  %rem64 = and i32 %y, 1
  %tobool36 = icmp eq i32 %rem64, 0
  br i1 %tobool36, label %if.else39, label %if.then37

if.then37:                                        ; preds = %for.end35
  %inc38 = add nsw i32 %x.1, 1
  br label %if.end41

if.else39:                                        ; preds = %for.end35
  %dec40 = add nsw i32 %x.1, -1
  br label %if.end41

if.end41:                                         ; preds = %if.else39, %if.then37
  %x.7 = phi i32 [ %inc38, %if.then37 ], [ %dec40, %if.else39 ]
  br label %for.inc42

for.inc42:                                        ; preds = %if.end41
  %inc43 = add nsw i32 %l1.0, 1
  br label %for.cond

for.end44:                                        ; preds = %for.cond
  ret i32 %x.0
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
