#lang eopl
(define list-set
  (lambda (lst n x)
    (cond
      [(null? lst) (eopl:error "List cannot be null")]
      [(zero? n) (cons x (cdr lst))]
      [else (cons (car x) (list-set (cdr lst) (- n 1) x))])))