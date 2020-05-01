#lang eopl
(require rackunit)

(define subst-in-s-exp
  (lambda (new old)
    (lambda (sexp)
      (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
          (subst new old sexp)))))

(define subst
  (lambda (new old slist)
    (map (subst-in-s-exp new old) slist)))

(check-equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))