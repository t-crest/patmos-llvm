; RUN: SUCCESS=true; \
; RUN: for n in {1..8}; do \
; RUN:  echo "n=$n"; \
; RUN:   sed "s/BASE_N/$n/g" %s > %t && opt -O2 %t -o - | llc | FileCheck %t \
; RUN:     && echo "  success" || SUCCESS=false; \
; RUN: done; $SUCCESS
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that multiplying using a constant that fits the following formula: 3*(2^n) 
; can be optimized onto a shadd instruction followed by a sl.
;
; This file tests for n = 1..8. It does so by first replacing 'BASE_N' with n.
; Then it calls 'opt' to optimize the formula calculation away and then compiles
; the resulting program to see if the multiply (which is the only thing ledt at this point)
; is turned into shadd and sl.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

; CHECK-LABEL: main:
define i32 @main(i32 %value)  {
entry:
  br label %loop
  
; Calculate 2^n. This is constant for each test, meaning the loop away leaving only the result.
loop: 
  %i = phi i32 [ 0, %entry], [ %i_next, %loop ]
  %pow = phi i32 [ 1, %entry], [ %pow_next, %loop ]
  %pow_next = mul nsw i32 %pow, 2
  %i_next = add nsw i32 %i, 1
  %cond = icmp ult i32 %i, BASE_N
  br i1 %cond, label %loop, label %loop_end
  
loop_end:
  %const = mul nsw i32 3, %pow ; (3 * 2^n)
  %0 = mul nsw i32 %value, %const 

; CHECK: shadd	$r[[R1:[0-9]+]]	=	$r[[R2:[0-9]+]],		$r[[R2]]
; CHECK: sl		$r{{[0-9]+}}	=	$r[[R1]],				BASE_N

  ret i32 %0
}

