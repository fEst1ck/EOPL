#lang eopl
(define swap-symbol
    (lambda (s1 s2 s)
      (cond
        [(eqv? s1 s) s2]
        [(eqv? s2 s) s1]
        [else s])))

(define swapper
  (lambda (s1 s2 slist)
    (cond
      [(null? slist) slist]
      [(symbol? slist) (swap-symbol s1 s2 slist)]
      [else (cons (swapper s1 s2 (car slist))
                  (swapper s1 s2 (cdr slist)))])))
      