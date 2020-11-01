(module project1 mzscheme
  ;;;;;;;;;;;; Comp 301 Project 1 ;;;;;;;;;;;;;;;;
  ;;; Add group members below
  ;;; Baran Berkay Hökelek, bhokelek16, 0060673
  ;;; Berkay Barlas, bbarlas15, 0054512
  ;;; Utku Noyan, unoyan16, 0060717
  ;;; save your file in the format: p1_0011221_asabuncuoglu13_00112222_galtintas17.rkt
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROJECT 1 Part A | Write your answer below here as a comment
  ;;
  ;;We are going to represent colors with given parameters R, G, B. We define two data representation such as hexadecimal and RGB color list.
  ;;
  ;;The grammar of  data representation hexadecimal as follows:
  ;;We can define the representation inductively by:
  ;;; <color>   ::= <color> <color> | <chexdigit>
  ;;; <chexdigit> ::= () | (cdigit.cidigit.cdigit.cidigit.cdigit.cidigit)
  ;;; <cdigit>  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | A | B | C | D | E | F
  ;;;
  ;;;
  ;;The grammar of  data representation RGB Color list as follows:
  ;;We can define the representation inductively by:
  ;;; <color>   ::= <color> <color> | <RGBlist>
  ;;; <RGBlist> ::= () | (RGBnum.RGBnum.RGBnum)
  ;;; <RGBnum>  ::= 0 | 1 | 2 | 3 .... | 255

  ;Second representation is ordered box representation:
  ;;
  ;;
  ;
  ;; PROJECT 1 Part B
  ;; First Representation | We added a suffix so that both first and second representations can be tested at once.

  (define create-hex-color
    (lambda (R G B)
      (append
       (num2hex R)
       (num2hex G)
       (num2hex B)
       )))

  (define num2hex
    (lambda (num)
      (cond
        ((> num 255) (list 'F 'F))
        ((< num 0) (list 0 0))
        (else (append (list (dec2hex (quotient num 16))) (list (dec2hex (remainder num 16)))))
        )))

  (define dec2hex
    (lambda (dec)
      (cond
        ((= dec 10) 'A)
        ((= dec 11) 'B)
        ((= dec 12) 'C)
        ((= dec 13) 'D)
        ((= dec 14) 'E)
        ((= dec 15) 'F)
        (else dec)
        )))

  (define is-zero-hex-color?
    (lambda (hex-color)
      (cond
        ((null? hex-color) #t)
        ((symbol? (car hex-color)) #f)
        ((zero? (car hex-color)) (is-zero-hex-color? (cdr hex-color)))
        (else #f)
        )))
  
 ; Successor increments each R, G, B values by 1.
  (define successor-hex-color
    (lambda (hex-color)
      (cond
        ((is-zero-hex-color? hex-color) (create-hex-color 0 0 1))
        ((= (length (reverse (plus1 (reverse hex-color)))) 7) (zeros 6))
        (else (reverse (plus1 (reverse hex-color))))
       )))

  (define plus1
    (lambda (lst)
      (cond
        ((null? lst) (list 1))
        ((not (pair? lst)) (p1val lst))
        ((eq? (car lst) 'F) (append (list 0) (plus1 (cdr lst))))
        (else (append (list (p1val (car lst))) (cdr lst)))
        )))

  (define p1val
    (lambda (lst)
      (cond ((eq? lst 'F) (list 0 1))  ((eq? lst 'A) 'B)     ((eq? lst 'B) 'C)    ((eq? lst 'C) 'D)    ((eq? lst 'D) 'E)    ((eq? lst 'E) 'F)    (else (+ lst 1)))))
  
  (define zeros
    (lambda (n)
      (if (= n 0) '() (append (list 0) (zeros (- n 1))))))
 

  ;; Second Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.
  ; Creates an rgb-color representation
  (define create-rgb-color
    (lambda (R G B)
      (list 
           (normalize-rgb-num R)
           (normalize-rgb-num G)
           (normalize-rgb-num B)
      )
    ))
  ; Check wheter given color is all zero/black
  (define is-zero-rgb-color?
     (lambda (rgb-color)
       (cond
         ((null? rgb-color) #t)
         ((= (car rgb-color) 0) (is-zero-rgb-color? (cdr rgb-color)))
         (else #f)
        )
       ))

  ; Successor increments each R, G, B values by 1.
  (define successor-rgb-color
    (lambda (rgb-color)
      (cond
        ((is-zero-rgb-color? rgb-color) (successor-rgb-color-helper '(0 0 0) 'r))
        (else (successor-rgb-color-helper rgb-color 'r))
        )
      ))
  ; Helper function to increment values by color
  (define successor-rgb-color-helper
    (lambda (rgb-color color)
      (cond
        ((equal? color 'r)
         (cons (successor-rgb-num (car rgb-color))
               (successor-rgb-color-helper (cdr rgb-color) 'g)))
        ((equal? color 'g)
         (cons (successor-rgb-num (car rgb-color))
               (successor-rgb-color-helper (cdr rgb-color) 'b)))
        ((equal? color 'b) (cons (successor-rgb-num (car rgb-color)) '()))
        (else #f) ; throw error
        )
      ))
  ; Successor of rgb-num, increment value by 1.
  (define successor-rgb-num
    (lambda (rgb-num)
      (cond
        ((= rgb-num 255) 0)
        ((is-in-rgb-num-range rgb-num) (+ 1 rgb-num))
        (else 0)
        )
      ))
  ; Helper function to check if rgb-num in range of 0..255
  (define is-in-rgb-num-range
    (lambda (rgb-num)
      (and (<= rgb-num 255) (>= rgb-num 0))
      ))

  ; Helper function to keep rgb-num in range
  (define normalize-rgb-num
    (lambda (rgb-num)
      (cond
        ((is-in-rgb-num-range rgb-num) rgb-num)
        ((> rgb-num 255) 255)
        ((< rgb-num 0) 0)
        )
      ))

  ;; PROJECT 1 Part C | Write your answer below here as a comment
  ;Constructors: Creates an instance of the datatype; the constructor takes as many arguments as the variant’s field-ids, and each argument is checked by applying the function produced by the variant’s predicate-expr.
  ;create-rgb-color, create-hex-color, successor-rgb-color, successor-hex-color, successor-rgb-num
  ;
  ;Observers: Extracts/Checks information from values of the data type. Has two kinds: predicates and extractors.
  ;is-zero-hex-color?, is-zero-rgb-color?
  ;
  ;Extractors: Extractors report an error if an expressed value is not of the expected kind and returns
  ; it if it's of expected kind.
  ;
  ;Predicates: Predicates takes information and returns boolean about the situation.
  ;is-zero-hex-color?, is-zero-rgb-color?
  
  ;
  
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  
  ;; PROJECT 1 Part D | Remove the comments and write test cases.
  (display "First Representation Tests\n")
  (equal?? (create-hex-color 0 0 0) '(0 0 0 0 0 0)) ; should return '(0 0 0 0 0 0)
  (equal?? (create-hex-color 99 99 99) '(6 3 6 3 6 3)) ; should return '(6 3 6 3 6 3)
  (equal?? (create-hex-color 255 200 155) '(F F C 8 9 B)) ; should return '('F 'F 'C 8 9 'B)
  (equal?? (create-hex-color 265 265 265) '(F F F F F F)) ; should return '('F 'F 'F 'F 'F 'F)
  (equal?? (create-hex-color -265 265 244) '(0 0 F F F 4)) ; should return '(0 0 'F 'F 'F 'E)
  (equal?? (is-zero-hex-color? '()) #t) ; should return #t
  (equal?? (is-zero-hex-color? '(0 0 0 0 0 0)) #t) ; should return #t
  (equal?? (is-zero-hex-color? '(0 0 0 0)) #t) ; should return #t
  (equal?? (is-zero-hex-color? '(0 0 1 0 0 0)) #f) ; should return #f
  (equal?? (is-zero-hex-color? '(1 0 0 0 1 0)) #f) ; should return #f
  (equal?? (is-zero-hex-color? '(A B C D E F)) #f) ; should return #f
  (equal?? (successor-hex-color '()) '(0 0 0 0 0 1)) ; should return '(1 1 1)
  (equal?? (successor-hex-color '(0 0 0 0 0 0)) '(0 0 0 0 0 1)) ; should return '(0 0 0 0 0 1)
  (equal?? (successor-hex-color '(0 0 1 8 6 4)) '(0 0 1 8 6 5)) ; should return '(0 0 1 8 6 4)
  (equal?? (successor-hex-color '(E F F F F F)) '(F 0 0 0 0 0)) ; should return '('F 'F 'F 'F 'F 'F)
  (equal?? (successor-hex-color '(F F F F F F)) '(0 0 0 0 0 0)) ; should return '(0 0 0 0 0 0)
  (newline)

  
  (display "Second Representation Tests\n")
  (equal?? (create-rgb-color 0 0 0) '(0 0 0)) ; should return '(0 0 0)
  (equal?? (create-rgb-color 99 99 99) '(99 99 99)) ; should return '(99 99 99)
  (equal?? (create-rgb-color 255 200 155) '(255 200 155)) ; should return '(255 200 155)
  (equal?? (create-rgb-color 265 265 265) '(255 255 255)) ; should return '(255 255 255)
  (equal?? (create-rgb-color -265 265 244) '(0 255 244)) ; should return '(255 255 255)
  (equal?? (is-zero-rgb-color? '()) #t) ; should return #t
  (equal?? (is-zero-rgb-color? '(0 0)) #t) ; should return #t
  (equal?? (is-zero-rgb-color? '(0 0 0)) #t) ; should return #t
  (equal?? (is-zero-rgb-color? '(0 0 1)) #f) ; should return #f
  (equal?? (is-zero-rgb-color? '(1 0 0)) #f) ; should return #f
  (equal?? (is-zero-rgb-color? '(0 244 0)) #f) ; should return #f
  (equal?? (successor-rgb-color '()) '(1 1 1)) ; should return '(1 1 1)
  (equal?? (successor-rgb-color '(0 0 0)) '(1 1 1)) ; should return '(1 1 1)
  (equal?? (successor-rgb-color '(10 10 10)) '(11 11 11)) ; should return '(11 11 11)
  (equal?? (successor-rgb-color '(254 254 254)) '(255 255 255)) ; should return '(255 255 255)
  (equal?? (successor-rgb-color '(255 255 255)) '(0 0 0)) ; should return '(0 0 0)
  (newline)
)