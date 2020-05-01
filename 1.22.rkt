#lang eopl
(define filter-in
  (lambda (pred lst)
    (if (null? lst) lst
        (if (pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
    