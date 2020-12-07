; RUN: LLC_ARGS="-mpatmos-max-subfunction-size=32"; \
; RUN: PASIM_ARGS="--mcsize=32"; \ 
; RUN: %test_no_runtime_execution
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
;
; Tests that setting the subfunction size to the same as the method cache size will work 
; successfully.
;
;//////////////////////////////////////////////////////////////////////////////////////////////////

@_1 = global i32 1
@_6 = global i32 6

; We make a function that is larger than 32 bytes, and cannot be
; optimized to a smaller size. This ensures the function will
; only execute correctly if it is split correctly.
define i32 @main()  {
entry:
  %_1 = load volatile i32* @_1
  %0 = add i32 1, %_1
  %_2 = load volatile i32* @_1
  %1 = add i32 %0, %_2
  %_3 = load volatile i32* @_1
  %2 = add i32 %1, %_3
  %_4 = load volatile i32* @_1
  %3 = add i32 %2, %_4
  %_5 = load volatile i32* @_1
  %4 = add i32 %3, %_5
  %_6 = load volatile i32* @_6
  %5 = sub i32 %4, %_6
  ret i32 %5 ; =0
}

