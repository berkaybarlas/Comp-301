(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
 (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))


       (newarray-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env)) (val2 (value-of exp2 env)))
                        (let ((length (expval->num val1)))
                          (letrec
                              ((populate-arr
                                (lambda (len val)
                                  (if (zero? len) '() (cons (newref val) (populate-arr (- len 1) val))))))
                            (arr-val (populate-arr length val2))))))

        (updatearray-exp (exp1 exp2 exp3)
                         (let ((val1 (value-of exp1 env))
                               (val2 (value-of exp2 env))
                               (val3 (value-of exp3 env)))
                           (let ((index (expval->num val2)))
                             (setref! (list-ref (expval->list val1) index) val3))))

        ; (val1 (map (lambda (x) (value-of x env)) exp1))
        (readarray-exp (exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                         (let ((index (expval->num val2)))
                           (deref (list-ref (expval->list val1) index)))))


       (newstack-exp ()
                     ; returns an empty stack
                     ;(create-stack 1000 -1) ; According to assumption I defined length
                     ;maximum possible and give each value -1 to understand where top is future functions
                     (letrec ((populate-arr
                                (lambda (len val)
                                  (if (zero? len) '() (cons (newref val) (populate-arr (- len 1) val))))))
                       (let ((proto-stack (populate-arr 1001 (num-val -1))))
                         (begin
                           (setref! (list-ref proto-stack 0) (num-val 0))
                           (arr-val proto-stack)))))

        (stack-push-exp (exp1 exp2)
                      ; adds the element val to the stack stk
                        (let ((stack-val (value-of exp1 env)) (el (value-of exp2 env)))
                          (let ((stack (expval->list stack-val)))
                            (let ((top (deref (list-ref stack 0))))
                              (begin
                                (setref! (list-ref stack (+ (expval->num top) 1)) el)
                                (setref! (list-ref stack 0) (num-val (+ (expval->num top) 1))))))))

        
        (stack-pop-exp (exp1)
                       ; removes the topmost element of the stack stk and returns its value.
                       (let ((stack-val (value-of exp1 env)))
                          (let ((stack (expval->list stack-val)))
                            (let ((top (deref (list-ref stack 0))))
                              (let ((top-el (deref (list-ref stack (expval->num top)))))
                              (begin
                                (setref! (list-ref stack (expval->num top)) (num-val -1))
                                (setref! (list-ref stack 0) (num-val (- (expval->num top) 1)))
                                top-el))))))
      
        (stack-size-exp (exp1)
                      ; returns the number of elements in the stack stk.
                        (let ((stack-val (value-of exp1 env)))
                          (let ((stack (expval->list stack-val)))
                            (let ((top (deref (list-ref stack 0))))
                              (num-val (expval->num top))))))
        
        (stack-top-exp (exp1)
                      ; returns the value of the topmost element in the stack stk without removal.
                       (let ((stack-val (value-of exp1 env)))
                          (let ((stack (expval->list stack-val)))
                            (let ((top (deref (list-ref stack 0))))
                              (deref (list-ref stack (expval->num top)))))))
        
        (empty-stack?-exp (exp1)
                      ; returns true if there is no element inside the stack stk and false otherwise.
                        (let ((stack-val (value-of exp1 env)))
                          (let ((stack (expval->list stack-val)))
                            (let ((top (deref (list-ref stack 0))))
                              (bool-val (eq? (expval->num top) 0))))))
        
        (print-stack-exp (exp1)
                      ; prints the elements in the stack stk.
                        (let ((stack-val (value-of exp1 env)))
                          (let ((stack (expval->list stack-val)))
                            (let ((top (deref (list-ref stack 0))))
                              (letrec ((display-single-character
                                        (lambda (stk ind)
                                          (if (= ind 1)
                                              (display (expval->num (deref (list-ref stk ind))))
                                              (begin
                                                (display (expval->num (deref (list-ref stk ind))))
                                                (display ", ")
                                                (display-single-character stk (- ind 1)))))))
                                (begin
                                  (display "(")
                                  (display-single-character stack (expval->num top))
                                  (display ")")
                                  (num-val 23)))))))
        (newqueue-exp ()
                    ; returns an empty queue
                    ;(create-queue 1000 -1) ; According to assumption of length
                    ; maximum possible and give each value -1 to understand where top is future functions
                       (letrec ((populate-arr
                                (lambda (len val)
                                  (if (zero? len) '() (cons (newref val) (populate-arr (- len 1) val))))))
                         (let ((queue-array (populate-arr 1001 (num-val -1))))
                           (begin
                             (setref! (list-ref queue-array 0) (num-val 0))
                             (arr-val queue-array)))))      
                   
        (queue-push-exp (exp1 exp2)
                        ;
                        (let ((queue-val (value-of exp1 env)) (el-val (value-of exp2 env)))
                          (let ((queue (expval->list queue-val)))
                            (let ((len (deref (list-ref queue 0))) (start-index (deref (list-ref queue 1))))
                              (begin
                                (setref! (list-ref queue (+ (expval->num len) (expval->num start-index))) el-val)
                                (setref! (list-ref queue 0) (num-val (+ (expval->num len) 1))))))))
                        
        (queue-pop-exp (exp1)
                       ; removes the first element of the queue stk and returns its value.
                       (let ((queue-val (value-of exp1 env)))
                          (let ((queue (expval->list queue-val)))
                            (let ((len (expval->num (deref (list-ref queue 0)))) (start-index (expval->num(deref (list-ref queue 1)))))
                              (let ((first-el (deref (list-ref queue (- (+ len start-index) 1)))))
                              (begin
                                (setref! (list-ref queue start-index) (num-val -1))
                                (setref! (list-ref queue 0) (num-val (- len 1)))
                                (setref! (list-ref queue 1) (num-val (+ start-index 1)))
                                first-el))))))
      
        (queue-size-exp (exp1)
                        ; returns the number of elements in the queue stk.
                        (let ((queue-val (value-of exp1 env)))
                          (let ((queue (expval->list queue-val)))
                            (let ((len (deref (list-ref queue 0))))
                              (num-val (expval->num len))))))
        (queue-top-exp (exp1)
                       ;
                       (let ((queue-val (value-of exp1 env)))
                          (let ((queue (expval->list queue-val)))
                            (let ((len (expval->num (deref (list-ref queue 0)))) (start-index (expval->num(deref (list-ref queue 1)))))
                              (deref (list-ref queue (- (+ len start-index) 1)))))))
    
   
        (empty-queue?-exp (exp1)
                        ; returns the number of elements in the queue stk.
                        (let ((queue-val (value-of exp1 env)))
                          (let ((queue (expval->list queue-val)))
                            (let ((len (deref (list-ref queue 0))))
                              (bool-val (eq? (expval->num len)))))))
      
        (print-queue-exp (exp1)
                      ; prints the elements in the queue stk.
                         (let ((queue-val (value-of exp1 env)))
                          (let ((queue (expval->list queue-val)))
                            (let ((top (deref (list-ref queue 0))))
                              (letrec ((display-single-character
                                        (lambda (stk ind)
                                          (if (= ind 1)
                                              (display (expval->num (deref (list-ref stk ind))))
                                              (begin
                                                (display (expval->num (deref (list-ref stk ind))))
                                                (display ", ")
                                                (display-single-character stk (- ind 1)))))))
                                (begin
                                  (display "(")
                                  (display-single-character queue (expval->num top))
                                  (display ")")
                                  (num-val 23)))))))
        

        )))

  

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )




  
