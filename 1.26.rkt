#lang racket
;; remove parentheses for e and add its contents to ls
(define combine
  (lambda (e ls)
    (cond
      [(null? e) ls]
      [(symbol? e) (cons e ls)]
      [else (cons (car e) (combine (cdr e) ls))])))

(define up
  (lambda (lst)
    (foldr combine '() lst)))