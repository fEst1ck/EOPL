#lang eopl
(require rackunit)

(define count-occurrences
  (lambda (s slist)
    (cond
      [(null? slist) 0]
      [(symbol? slist)
       (if (eqv? s slist) 1 0)]
      [else (+ (count-occurrences s (car slist))
               (count-occurrences s (cdr slist)))])))

(check-equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)
(check-equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
(check-equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)