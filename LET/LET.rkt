#lang eopl
;; 
(define scanner-spec
  '((white-spce (whitespace) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define grammar
  '((program
     (expression)
     a-program)
    
    (expression
     (number)
     const-exp)
    
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression
     (identifier)
     var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

;; Syntax data types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype program program?
  [a-program
   (exp1 expression?)])

(define-datatype expression expression?
  [const-exp
   (num number?)]
  [diff-exp
   (exp1 expression?)
   (exp2 expression?)]
  [zero?-exp
   (exp1 expression?)]
  [if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?)]
  [var-exp
   (var identifier?)]
  [let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?)])

(define identifier? symbol?)


;; Expresses values ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  [num-val
   (num number?)]
  [bool-val
   (bool boolean?)])

;; ExpVal -> Int
(define (expval->num val)
  (cases expval val
    [num-val (num) num]
    [else (report-expval-extractor-error 'num val)]))

;; ExpVal -> Bool
(define (expval->bool val)
  (cases expval val
    [bool-val (bool) bool]
    [else (report-expval-extractor-error 'bool val)]))

(define (report-expval-extractor-error error-type val)
  (eopl:error error-type "incorrect type for ~s" val))


;; Environment data types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype env env?
  [empty-env]
  [extend-env
   (id identifier?)
   (val expression?)
   (parent-env env?)])

;; Env Identifier -> Expression
(define (apply-env e search-var)
  (cases env e
    [empty-env ()
      (eopl:error 'apply-env "unbound variable ~s" search-var)]
    [extend-env (id val parent)
      (if (eqv? id search-var) val
          (apply-env parent search-var))]
    [else (eopl:error 'apply-env "unvalid environment ~s" e)]))

;; () -> Env
(define (init-env)
  (extend-env
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))

;; Interpreter for let ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String -> ExpVal
(define (run string)
  (value-of-program (scan&parse string)))

;; Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    [a-program (exp)
      (value-of exp (init-env))]))

;; Exp Env -> ExpVal
(define (value-of exp env)
  (cases expression exp
    [const-exp (num)
    (num-val num)]
    [diff-exp (exp1 exp2)
      (num-val (- (expval->num (value-of exp1 env))
                  (expval->num (value-of exp2 env))))]
    [zero?-exp (exp1)
     (let ([val (value-of exp1 env)])
       (let ([num (expval->num val)])
         (bool-val (zero? num))))]
    [if-exp (pred exp1 exp2)
      (if (expval->bool (value-of pred env))
          (value-of exp1 env)
          (value-of exp2 env))]
    [var-exp (var)
      (apply-env env var)]
    [let-exp (var exp1 body)
      (let ([new-env (extend-env var (value-of exp1 env))])
        (value-of body new-env))]))
      