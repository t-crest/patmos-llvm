; RUN: llc < %s -mpatmos-basicblock-align=2 | FileCheck %s
; RUN: llc < %s -mpatmos-basicblock-align=2 -filetype=obj -o %t -mforce-block-labels;\
; RUN: patmos-ld %t -nostdlib -static -o %t --section-start .text=1;\
; RUN: llvm-objdump %t -d | FileCheck %s --check-prefix ALIGN
; RUN: LLC_ARGS="-mpatmos-basicblock-align=2"; %test_no_runtime_execution
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that we can change the basic block alignment to 2 bytes.
;
; We check this is two ways:
;	1. We ensure that when outputting assembly, the basic blocks are preceded by the alignment. 
;	2. We check that when outputting object code the address of the first instuction
;		in the function is aligned correctly.
;		We use patmos-ld to start the .text section at address 1, 
;		which is never a valid alignment.
;		We then to the check using llvm-objdump and the "ALIGN" prefix for FileCheck.
;		We then check that the address after the main label ends in the correct values
;		fitting the alignment.
;	3. We check that the program executes correctly for good measure
;		
; The program simply loads a volatile value (7).
; If the value is 7, 0 is returned, otherwise returns value+2.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_7 = global i32 7

; CHECK-LABEL: main:
define i32 @main()  {

; We don't want any alignment label between the function label and the entry
; block's label, as that is redundant

; CHECK-NOT: .align
; CHECK-LABEL: # %entry
entry:
  %0 = load volatile i32* @_7
  %tobool = icmp eq i32 %0, 7
  br i1 %tobool, label %if.else, label %if.then

; CHECK: .align 2
; CHECK-NEXT: # %if.else

; ALIGN: .LBB0_{{[1-2]}}
; ALIGN-NEXT: {{[0-9]+[0|2|4|6|8|a|c|e]}}:
if.else:                                          ; preds = %entry
  %1 = sub nsw i32 %0, 7
  br label %if.end
  
; CHECK: .align 2
; CHECK-NEXT: # %if.then

; ALIGN: .LBB0_{{[1-2]}}
; ALIGN-NEXT: {{[0-9]+[0|2|4|6|8|a|c|e]}}:
if.then:                                          ; preds = %entry
  %2 = add nsw i32 %0, 2
  br label %if.end


if.end:                                           ; preds = %if.else, %if.then
  %result = phi i32 [ %2, %if.then ], [ %1, %if.else ]
  ret i32 %result
}
