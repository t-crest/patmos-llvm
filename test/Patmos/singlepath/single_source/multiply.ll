; RUN: %test_singlepath_execution -O2 0=1 1=2 2=5 3=10 10=101
; RUN: %test_singlepath_execution "-O2 -mpatmos-disable-vliw=false" 0=1 1=2 2=5 3=10 10=101
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
; 
; Tests a simple expression which includes a multiply.
; The following is the equivalent C code:
;
; int main(int x){
; 	return (x * x) + 1;
; }
; 
;//////////////////////////////////////////////////////////////////////////////////////////////////

define i32 @main(i32 %x) {
entry:
  %mul = mul nsw i32 %x, %x
  %add = add nsw i32 %mul, 1
  ret i32 %add
}
