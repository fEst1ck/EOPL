#lang eopl
(define flatten
  (lambda (slist)
    (cond
      [(null? slist) slist]
      [(list? (car slist)) (append (flatten (car slist))
                                   (flatten (cdr slist)))]
      [else (cons (car slist) (flatten (cdr slist)))])))