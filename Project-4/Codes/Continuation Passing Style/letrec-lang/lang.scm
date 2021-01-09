(module lang (lib "eopl.ss" "eopl")                
  
  ;; grammar for the LETREC language

  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
        ("letrec"
          identifier "(" identifier ")" "=" expression
           "in" expression)
        letrec-exp)

      ;;;;;;;; TASK 4: LISTS ;;;;;;;
      ; implement car(expression)      
      (expression
       ("car" "(" expression ")")
       car-exp)
      ; implement cdr(expression)      
      (expression
       ("cdr" "(" expression ")")
       cdr-exp)
      ; implement null?(expression)      
      (expression
       ("null?" "(" expression ")")
       null?-exp)
      ; implement emptylist      
      (expression
       ("emptylist")
       emptylist-exp)
      ; implement list(exp1, exp2, ..., expN)
      ; hint: use seperate-list function of scheme.
      (expression
       ("list" (separated-list expression ","))
       list-exp)
      
      ; implement map(expression, expression)
      ; the first expression should be treated as a proc expression with one parameter.
      ; the second expression should be treated as a list expression.
      (expression
       ("map" "(" expression "," expression ")")
       map-exp)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
