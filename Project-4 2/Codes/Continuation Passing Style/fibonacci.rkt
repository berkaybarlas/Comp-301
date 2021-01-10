#lang racket

;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;

(define fibo-cont
  (lambda (n c)
    (if (<= n 2)
        (c 1)
        (fibo-cont (- n 1)
        (lambda (x1)
          (fibo-cont (- n 2) (lambda (x2) (c (+ x1 x2)))))))))
       

(define fibonacci
  (lambda (n)
    (fibo-cont n (lambda (x) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; i:    1  2  3  4  5  6  7  8  9  10 11 ...
; f(i): 1  1  2  3  5  8  13 21 34 55 ...

;; Tests
(display (fibonacci 4)) ; should output 3
(display  "\n")
(display (fibonacci 7)) ; should output 13
(display  "\n")
(display (fibonacci 8)) ; should output 21
(display  "\n")