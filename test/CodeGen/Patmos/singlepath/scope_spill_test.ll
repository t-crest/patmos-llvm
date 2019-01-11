; RUN: llc  < %s -mpatmos-singlepath=init_func  | FileCheck %s 

;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests the spilling of the predicate register when entering new loop scopes.
; The following is the equivalent C code:
;
;	volatile int x = 0;
;	volatile int y = 10;
;
;	void init_func(){
;		_Pragma("loopbound min 0 max 10")
;		for(int l1 = 0; l1<y; l1++){
;			int g1 = y!=0;
;			_Pragma("loopbound min 0 max 20")
;			for(int l2 = 0; l2<y; l2++){
;				int g2 = y!=0;
;				_Pragma("loopbound min 0 max 30")
;				for(int l3 = 0; l3<y; l3++){
;					int g3 = y!=0;
;					_Pragma("loopbound min 0 max 40")
;					for(int l4 = 0; l4<y; l4++){
;						if(y!=0) {x++;}else{y++;};
;					}
;					if(g3) {x++;}else{y++;};
;				}
;				if(g2) {x++;}else{y++;};
;			}
;			if(g1) {x++;}else{y++;};
;		}
;	}
;
; We use 4 nested loops, with each loop calculating a predicate, before the inner loop starts ,
; that must be used after the inner loop ends. Each loop therefore needs 2 predicates to be 
; maintained through the execution of the inner loop. Therefore, since 2*4=8 and we only have
; 6 predicate registers, at least 1 spill of the predicate registers is required.
; The below test does not test the if/else statement's generated code, only code relevant
; to iterating through the loops.
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////
target triple = "patmos-unknown-unknown-elf"

@x = global i32 0
@y = global i32 10

define void @init_func() #0 {
entry:
  br label %for1.cond

; CHECK-LABEL: init_func:
; CHECK:						pmov	$p2 = $p0
; CHECK:						mov		$r3 = $r0
; CHECK:						li		$r2 = x
; CHECK:						li		$r1 = y
; CHECK:						li		$r26 = 11
; CHECK:						sws		[0] = $r26


for1.cond:                                         ; preds = %for1.inc, %entry
  %l1.0 = phi i32 [ 0, %entry ], [ %inc46, %for1.inc ]
  %0 = load volatile i32* @y
  call void @llvm.loopbound(i32 0, i32 10)
  %cmp = icmp slt i32 %l1.0, %0
  br i1 %cmp, label %for1.body.start, label %for1.end

; CHECK-LABEL: .LBB0_1:
; CHECK:			( $p2)		lwc		$r4 = [$r1]
; CHECK:						nop	
; CHECK:			( $p2)		cmple	$p1 = $r4, $r3
; CHECK:						pand	$p3 =  $p2, !$p1


for1.body.start:                                         ; preds = %for1.cond
  %1 = load volatile i32* @y
  %cmp1 = icmp eq i32 %1, 0
  br label %for2.cond
  
; CHECK:			( $p3)		lwc		$r4 = [$r1]
; CHECK:			( $p2)		pmov	$p2 = !$p1
; CHECK:						mfs		$r26 = $s0
; CHECK:						sbs		[17] = $r26
; CHECK:						li		$r26 = 21
; CHECK:						sws		[1] = $r26
; CHECK:						pmov	$p2 =  $p3
; CHECK:			( $p3)		mov		$r5 = $r0

for2.cond:                                        ; preds = %for2.inc, %for1.body.start
  %l2.0 = phi i32 [ 0, %for1.body.start ], [ %inc37, %for2.inc ]
  %2 = load volatile i32* @y
  call void @llvm.loopbound(i32 0, i32 20)
  %cmp3 = icmp slt i32 %l2.0, %2
  br i1 %cmp3, label %for2.body.start, label %for2.end
  
; CHECK-LABEL: .LBB0_2:
; CHECK:			( $p2)		lwc		$r6 = [$r1]
; CHECK:						nop
; CHECK:			( $p2)		cmple	$p1 = $r6, $r5
; CHECK:						pand	$p3 =  $p2, !$p1

for2.body.start:                                        ; preds = %for2.cond
  %3 = load volatile i32* @y
  %cmp6 = icmp eq i32 %3, 0
  br label %for3.cond

; CHECK:			( $p3)		lwc		$r6 = [$r1]
; CHECK:			( $p2)		pmov	$p2 = !$p1
; CHECK:						mfs		$r26 = $s0
; CHECK:						sbs		[18] = $r26
; CHECK:						li		$r26 = 31
; CHECK:						sws		[2] = $r26
; CHECK:						pmov	$p2 =  $p3
; CHECK:			( $p3)		mov		$r7 = $r0

for3.cond:                                        ; preds = %for3.inc, %for2.body.start
  %l3.0 = phi i32 [ 0, %for2.body.start ], [ %inc28, %for3.inc ]
  %4 = load volatile i32* @y
  call void @llvm.loopbound(i32 0, i32 30)
  %cmp9 = icmp slt i32 %l3.0, %4
  br i1 %cmp9, label %for3.body.start, label %for3.end

; CHECK-LABEL: .LBB0_3:
; CHECK:			( $p2)		lwc		$r8 = [$r1]
; CHECK:						nop
; CHECK:			( $p2)		cmple	$p1 = $r8, $r7
; CHECK:						pand	$p3 =  $p2, !$p1


for3.body.start:                                       ; preds = %for3.cond
  %5 = load volatile i32* @y
  %cmp12 = icmp eq i32 %5, 0
  br label %for4.cond
  
; CHECK:			( $p3)		li		$r9 = -1
; CHECK:			( $p3)		lwc		$r8 = [$r1]
; CHECK:			( $p2)		pmov	$p2 = !$p1
; CHECK:						mfs		$r26 = $s0
; CHECK:						sbs		[19] = $r26
; CHECK:						li		$r26 = 41
; CHECK:						sws		[3] = $r26
; CHECK:						pmov	$p2 =  $p3

for4.cond:                                       ; preds = %for4.inc, %for3.body.start
  %l4.0 = phi i32 [ 0, %for3.body.start ], [ %inc21, %for4.inc ]
  %6 = load volatile i32* @y
  call void @llvm.loopbound(i32 0, i32 40)
  %cmp15 = icmp slt i32 %l4.0, %6
  br i1 %cmp15, label %for4.body.start, label %for4.end

; CHECK-LABEL: .LBB0_4:
; CHECK:			( $p2)		lwc		$r10 = [$r1]
; CHECK:			( $p2)		add		$r9 = $r9, 1
; CHECK:			( $p2)		cmple	$p1 = $r10, $r9
; CHECK:						pand	$p3 =  $p2, !$p1

for4.body.start:                                       ; preds = %for4.cond
  %7 = load volatile i32* @y
  %cmp18 = icmp eq i32 %7, 0
  br i1 %cmp18, label %for4.body.if.false, label %for4.body.if.true
 
for4.body.if.true:                                          ; preds = %for4.body.start
  %8 = load volatile i32* @x
  %inc = add nsw i32 %8, 1
  store volatile i32 %inc, i32* @x
  br label %for4.body.end

for4.body.if.false:                                          ; preds = %for4.body.start
  %9 = load volatile i32* @y
  %inc20 = add nsw i32 %9, 1
  store volatile i32 %inc20, i32* @y
  br label %for4.body.end

for4.body.end:                                           ; preds = %for4.body.if.false, %for4.body.if.true
  br label %for4.inc

for4.inc:                                          ; preds = %for4.body.end
  %inc21 = add nsw i32 %l4.0, 1
  br label %for4.cond
  
; CHECK:						sub		$r26 = $r26, 1
; CHECK:						cmplt	$p7 = $r0, $r26	
; CHECK:			( $p7)		br		.LBB0_4	

for4.end:                                          ; preds = %for4.cond
  br i1 %cmp12, label %for3.body.if.false, label %for3.body.if.true
  
; CHECK:						lbs		$r26 = [19]
; CHECK:						nop
; CHECK:						mts		$s0 = $r26


for3.body.if.true:                                        ; preds = %for4.end
  %10 = load volatile i32* @x
  %inc23 = add nsw i32 %10, 1
  store volatile i32 %inc23, i32* @x
  br label %for3.body.end

for3.body.if.false:                                        ; preds = %for4.end
  %11 = load volatile i32* @y
  %inc25 = add nsw i32 %11, 1
  store volatile i32 %inc25, i32* @y
  br label %for3.body.end

for3.body.end:                                         ; preds = %for3.body.if.false, %for3.body.if.true
  br label %for3.inc

for3.inc:                                        ; preds = %for3.body.end
  %inc28 = add nsw i32 %l3.0, 1
  br label %for3.cond

; CHECK:						lws		$r26 = [2]
; CHECK:						sub		$r26 = $r26, 1
; CHECK:						cmplt	$p7 = $r0, $r26	
; CHECK:			( $p7)		br		.LBB0_3

for3.end:                                        ; preds = %for3.cond
  br i1 %cmp6, label %for2.body.if.false, label %for2.body.if.true

; CHECK:						lbs	$r26 = [18]
; CHECK:						nop	
; CHECK:						mts	$s0 = $r26

for2.body.if.true:                                        ; preds = %for3.end
  %12 = load volatile i32* @x
  %inc32 = add nsw i32 %12, 1
  store volatile i32 %inc32, i32* @x
  br label %for2.body.end

for2.body.if.false:                                        ; preds = %for3.end
  %13 = load volatile i32* @y
  %inc34 = add nsw i32 %13, 1
  store volatile i32 %inc34, i32* @y
  br label %for2.body.end

for2.body.end:                                         ; preds = %for2.body.if.false, %for2.body.if.true
  br label %for2.inc

for2.inc:                                        ; preds = %for2.body.end
  %inc37 = add nsw i32 %l2.0, 1
  br label %for2.cond

; CHECK:						lws		$r26 = [1]
; CHECK:						sub		$r26 = $r26, 1
; CHECK:						cmplt	$p7 = $r0, $r26
; CHECK:			( $p7)		brcf	.LBB0_2	

for2.end:                                        ; preds = %for2.cond
  br i1 %cmp1, label %for1.body.if.false, label %for1.body.if.true

; CHECK:						lbs	$r26 = [17]
; CHECK:						nop	
; CHECK:						mts	$s0 = $r26

for1.body.if.true:                                        ; preds = %for2.end
  %14 = load volatile i32* @x
  %inc41 = add nsw i32 %14, 1
  store volatile i32 %inc41, i32* @x
  br label %for1.body.end

for1.body.if.false:                                        ; preds = %for2.end
  %15 = load volatile i32* @y
  %inc43 = add nsw i32 %15, 1
  store volatile i32 %inc43, i32* @y
  br label %for1.body.end

for1.body.end:                                         ; preds = %for1.body.if.false, %for1.body.if.true
  br label %for1.inc

for1.inc:                                        ; preds = %for1.body.end
  %inc46 = add nsw i32 %l1.0, 1
  br label %for1.cond

; CHECK:						lws		$r26 = [0]
; CHECK:						sub		$r26 = $r26, 1
; CHECK:						cmplt	$p7 = $r0, $r26
; CHECK:			( $p7)		brcf	.LBB0_1

for1.end:                                        ; preds = %for1.cond
  ret void
}

declare void @llvm.loopbound(i32, i32) #1
