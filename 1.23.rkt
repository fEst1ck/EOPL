#lang eopl
(define list-index
  (lambda (pred lst)
    (if (null? lst) #f
        (if (pred (car lst)) 0
            (let ([index (list-index pred (cdr lst))])
              (if index (+ 1 index)
                  index))))))