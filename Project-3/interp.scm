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
                     (newarray-exp 1000 -1) ; According to assumption I defined length
                     ;maximum possible and give each value -1 to understand where top is future functions
                      )

        (stack-push-exp (exp1 exp2)
                      ; adds the element val to the stack stk
                        (define find-top
                          (lambda (arr count)
                          (if (eq? (readarray-exp arr (num-val count)) -1)
                              0
                              (+ 1 (find-top (arr (+ count 1))))))
                          )
                      (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env))
                             (top-idx (find-top exp1 0)))
                         (updatearray-exp (exp1 top-idx exp2)))
                      )

      
        (stack-pop-exp (exp1)
                       ; removes the topmost element of the stack stk and returns its value.
                       (define find-top
                         (lambda (arr count)
                           (if (eq? (readarray-exp arr count) -1)
                               0
                               (+ 1 (find-top (arr (+ count 1))))))
                         )
                       (let ((stck (value-of exp1 env))
                             (top-idx (find-top exp1 0)))
                         (let ((pop-val (readarray-exp exp1 top-idx)))
                           (begin (updatearray-exp (exp1 top-idx -1))
                                  (num-val pop-val)
                                  )))
                       )
      
        (stack-size-exp (exp1)
                      ; returns the number of elements in the stack stk.
                        (define find-top
                          (lambda (arr count)
                          (if (eq? (readarray-exp arr count) -1)
                              0
                              (+ 1 (find-top (arr (+ count 1))))))
                          )
                        (num-val (find-top exp1 0))
                        )
        
        (stack-top-exp (exp1)
                      ; returns the value of the topmost element in the stack stk without removal.
                       (define find-top
                          (lambda (arr count)
                          (if (eq? (readarray-exp arr count) -1)
                              0
                              (+ 1 (find-top (arr (+ count 1))))))
                          )
                       (let ((val1 (value-of exp1 env))
                             (top-idx (find-top exp1 0)))
                       (expval->ref (readarray-exp exp1 top-idx)))
                      )
        
        (empty-stack?-exp (exp1)
                      ; returns true if there is no element inside the stack stk and false otherwise.
                          (define find-top
                          (lambda (arr count)
                          (if (eq? (readarray-exp arr count) -1)
                              0
                              (+ 1 (find-top (arr (+ count 1))))))
                          )
                          (let ((val1 (value-of exp1 env))
                             (top-idx (find-top exp1 0)))
                            (if (zero? top-idx)
                                (bool-val #t)
                                (bool-val #f)))
                      )
        
        (print-stack-exp (exp1)
                      ; prints the elements in the stack stk.
                         (define find-top
                          (lambda (arr count)
                          (if (eq? (readarray-exp arr count) -1)
                              0
                              (+ 1 (find-top (arr (+ count 1))))))
                          )
                         (let ((stck (value-of exp1 env))
                             (top-idx (find-top exp1 0)))
                           (if (zero? top-idx) '()
                               (display stck)))
                      )
        

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




  
