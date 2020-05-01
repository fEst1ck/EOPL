#lang eopl
(require rackunit)
;; subst : Sym Sym S-list -> S-list
(define subst
  (lambda (new old slist)
    (cond
      [(null? slist) slist]
      [(symbol? slist) (if (eqv? old slist) new slist)]
      [else (cons (subst new old (car slist)) (subst new old (cdr slist)))])))

(check-equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))