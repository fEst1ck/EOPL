#lang eopl
(define invert
  (lambda (lst)
    (map (lambda (2-lst) `(,(cadr 2-lst) ,(car 2-lst)))
         lst)))