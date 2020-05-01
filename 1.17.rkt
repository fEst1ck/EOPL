#lang eopl
(define down
  (lambda (lst)
    (map (lambda (x) `(,x)) lst)))