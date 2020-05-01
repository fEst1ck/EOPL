#lang eopl

;; var-exp : Var -> Lc-exp
(define (var-exp var)
  (if (symbol? var) var
      (eopl:error 'var-exp "~s must be a symbol" var)))

;; lambda-exp : Var Lc-exp -> Lc-exp
(define (lambda-exp var lc-exp)
  (if (and (var-exp? var) (lc-exp? exp))
       `(lambda (,var) ,lc-exp)
       (eopl:error 'lambda-exp "invalid input")))
 

;; Lc-exp Lc-exp -> Lc-exp
(define (app-exp exp1 exp2)
  (if (and (lc-exp? exp1) (lc-exp? exp2))
      `(,exp1 ,exp2)
      (eopl:error 'app-exp "invalid input")))

;; Lc-exp -> Bool
(define var-exp? symbol?)

;; Lc-exp -> Bool
(define (lambda-exp? exp)
  (and
   (list? exp)
   (not (null? exp))
   (eqv? (car exp) 'lambda)
   (not (null? (cdr exp)))
   (list? (cadr exp))
   (var-exp? (lambda-exp->bound-var exp))
   (not (null? (cddr exp)))
   (lc-exp? (lambda-exp->body exp))
   (null? (cdddr exp))))

;; Lc-exp -> Bool
(define (app-exp? exp)
  (and
   (list? exp)
   (not (null? exp))
   (lc-exp? (app-exp->rator exp))
   (not (null? (cdr exp)))
   (lc-exp? (app-exp->rand exp))
   (null? (cddr exp))))

;; Lc-exp -> Bool
(define (lc-exp? exp)
  (or (var-exp? exp)
      (lambda-exp? exp)
      (app-exp? exp)))

;; Lc-exp -> Var
(define (var-exp->var exp)
  exp)

;; Lc-exp -> Var
(define (lambda-exp->bound-var exp)
  (caadr exp))

;; Lc-exp -> Lc-exp
(define (lambda-exp->body exp)
  (caddr exp))

;; Lc-exp -> Lc-exp
(define (app-exp->rator exp)
  (car exp))

;; Lc-exp -> Lc-exp
(define (app-exp->rand exp)
  (cadr exp))