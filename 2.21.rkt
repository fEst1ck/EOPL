#lang eopl
(define (SchemeVal? exp)
  #t)

(define-datatype Env Env?
  (empty-env)
  (extend-env
   (id symbol?)
   (val SchemeVal?)
   (parent-env Env?)))

;; Env Id -> SchemeVal
(define (apply-env env search-var)
  (cases Env env
    (empty-env ()
      (eopl:error 'apply-env "unbound variable ~s" search-var))
    (extend-env (id val parent-env)
       (if (eqv? id search-var) val
           (apply-env parent-env search-var)))))

;; Env Id -> Bool
(define (has-binding? env search-var)
  (cases Env env
    [empty-env () #f]
    [extend-env (id val parent-env)
      (if (eqv? id search-var) #t
          (has-binding? parent-env search-var))]))