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

      ;; --- Call-by-reference and Call-by-name here
      (cbr-vs-cbname
       "let or = proc(s1) if zero?(-(s1, 1)) then proc(s2) 1 else proc(s2) if zero?(-(s2, 1)) then 1 else 0
            in let exp1 = 1
                   in let exp2 = proc(x) begin set x = 0; 0 end
                          in let x = 1 in begin ((or exp1) (exp2 x)); x end" 1)
                 
      ;; --- Call-by-value and Call-by-name here
      (cbv-vs-cbname
       "let k = 3
            in let p = proc(x) k
                   in let f = proc(x) begin set x = 5; 12 end
                          in begin set k = 1; (p (f k)) end" 1)

      ;; ======================= PARAMETER PASSING - TASK 3 ========================
    )
  )
)