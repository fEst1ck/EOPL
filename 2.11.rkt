#lang eopl
;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;; Var = Sym

;; empty-env : () -> Env
(define empty-env (lambda () '()))

;; extend-env : Var SchemeVal Env -> Env
(define extend-env
  (lambda (var val env)
    (cons
     (cons (list var) (list val))
     env)))

;; apply-env : Env Var -> SchemeVal
(define (apply-env env search-var)
  (define helper
    (lambda (lovar loval search-var)
      (cond
        [(null? lovar) #f]
        [(eqv? (car lovar) search-var) (cons (car lovar) (car loval))]
        [else (helper (cdr lovar) (cdr loval) search-var)])))
  (if (null? env)
      (eopl:error 'apply-env "unbound variable ~s" search-var)
      (let ([var-val (helper (caar env) (cdar env) search-var)])
        (if var-val (car var-val)
            (apply-env (cdr env) search-var)))))
;; Listof(Var) Listof(Val) -> Env
(define extend-env*
  (lambda (lovar loval env)
    (cons (cons lovar loval)
          env)))