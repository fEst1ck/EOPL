#lang racket
(define product
  (lambda (sos1 sos2)
    (if (null? sos1) '()
        (foldl (lambda (new acc) (cons `(,(car sos1) ,new) acc))
           (product (cdr sos1) sos2)
           sos2))))