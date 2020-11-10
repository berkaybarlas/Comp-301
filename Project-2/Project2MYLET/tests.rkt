#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interp.rkt")           ; for value-of-program

;; run : String -> ExpVal
;; Page: 71
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval 
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      (else
       (eopl:error 'sloppy->expval 
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-run
 ;; simple arithmetic
 (positive-const "11" 11)
 (zero "0" 0)
 
; ;; check environment
; (env-i "i" error)
; ;;; replace these with the values you defined
; (env-x "x" 4)
; (env-y "y" 2)
; (env-z "z" 3)
; 
; ;; simple unbound variables
; (test-unbound-var-1 "foo" error)
; (test-unbound-var-2 "-(x,foo)" error)
; 
; ;; test dynamic typechecking
; (no-bool-to-diff-1 "op(zero?(0),1, 4)" error)
; (no-bool-to-sum-2 "op(1,zero?(0), 1)" error)
; (no-int-to-if "if 1 then 2 else 3" error)
; (no-string-to-if "if 'gul' then 'sena' else 'altintas'" error)
;
; ;; make sure that the test and both arms get evaluated
; ;; properly. 
; (if-eval-test-true "if zero?(op(11,11, 4)) then 3 else 4" 3)
; (if-eval-test-false "if zero?(op(11, 12, 4)) then 3 else 4" 4)
; 
; ;; and make sure the other arm doesn't get evaluated.
; (if-eval-test-true-2 "if zero?(op(11, 11, 4)) then 3 else foo" 3)
; (if-eval-test-false-2 "if zero?(op(11,12, 4)) then foo else 4" 4)
; 
; ;; simple let
; (simple-let-1 "let x = 3 in x" 3)
; 
; ;; make sure the body and rhs get evaluated
; (eval-let-body "let x = 3 in op(x,1,4)" 2)
; (eval-let-rhs "let x = op(4,1,1) in op(x,1,4)" 4)
; 
; ;; check nested let and shadowing
; (simple-nested-let "let x = 3 in let y = 4 in op(x,y, 4)" -1)
; (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
; (check-shadowing-in-rhs "let x = 3 in let x = op(x,1,1) in x" 4)
;
; ;;;; basic if tests
; (if-basic "if zero?(0) then 2 else 1" 2)
; (else-basic "if zero?(3) then 2 else 1" 1)
; (if-elif-basic "if zero?(3) then 3 elif zero?(4) then 4 elif zero?(0) then 1 else 2" 1)
; (else-elif-basic "if zero?(3) then 3 elif zero?(4) then 4 elif zero?(1) then 1 else 2" 2)
;
; ;;; op exps
; (op-test-sum "op(3, 2, 1)" 5)
; (op-test-mult "op(3, 2, 2)" 6)
; (op-test-div "op(3, 2, 3)" 3/2)
; (op-test-diff "op(3, 2, 4)" 1)
;
; ;;; complex tests combining op and if
; (elif-test "if zero?(op(3, 3, 1)) then op(3, 3, 1) elif zero?(op(3, 3, 2)) then op(3, 3, 2) elif zero?(op(3, 3, 4)) then op(3, 3, 4) else 3" 0)
; (else-test "if zero?(op(3, 3, 1)) then op(3, 3, 1) elif zero?(op(3, 3, 2)) then op(3, 3, 2) elif zero?(op(3, 3, 2)) then op(3, 3, 4) else 3" 3)
; (if-test "if zero?(op(3, 3, 1)) then op(3, 3, 1) else 3" 3)
; (if-true-test "if zero?(op(3, 3, 4)) then op(3, 3, 1) else 3" 6)
;
; ;;; string tests
; (str-test "'this'" "'this'")
; (str-test2 "'test" error)
; 
 )