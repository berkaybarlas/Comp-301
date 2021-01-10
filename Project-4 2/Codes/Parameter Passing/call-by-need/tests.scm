(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)


      ;; ======================= PARAMETER PASSING - TASK 3 ========================

      ;; Write the expression that evaluates different for:

      ;; --- Call-by-reference and Call-by-need here

      (cbr-vs-cbneed "let p = proc(x) 11
                          in let f = proc(x) begin set x = 5; 12 end
                              in let x = 3 in begin (p (f x)); x end" 3)

      ;; --- Call-by-value and Call-by-need here
      (cbv-vs-cbneed
       "let k = 0
            in let p1 = proc(x) -(-(1,-1), k)
                   in let p2 = proc(x) if zero?(x) then begin set x = 27; x end else x
                          in begin set k = 6; (p1 (p2 k)) end" -4)
      

      ;; ======================= PARAMETER PASSING - TASK 3 ========================
    )
  )
)