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

;; Listof(Var) Listof(Val) -> Env
(define extend-env*
  (lambda (lovar loval env)
    (if (null? lovar)
        (if (null? loval)
            env
            (eopl:error 'extend-env* "ListofVar and Listofval must be of same length"))
        (if (null? loval)
            (eopl:error 'extend-env* "ListofVar and Listofval must be of same length")
            (append
              (list (cons (car lovar) (car loval)))
              (extend-env*
               (cdr lovar)
               (cdr loval)
                env))))))