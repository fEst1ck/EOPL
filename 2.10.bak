#lang eopl
;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;; Var = Sym

;; empty-env : () -> Env
(define empty-env (lambda () '()))

;; extend-env : Var SchemeVal Env -> Env
(define extend-env
  (lambda (var val env)
    (cons (cons var val)
          env)))

;; apply-env : Env Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (let [(val (assoc search-var env))]
      (if val (cdr val)
          (eopl:error 'apply-env "No binding for ~s" search-var)))))