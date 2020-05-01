#lang racket
(define merge-helper
  (lambda (loi1 loi2 acc)
    (cond
      [(null? loi1) (foldl cons acc loi2)]
      [(null? loi2) (foldl cons acc loi1)]
      [(< (car loi1) (car loi2))
       (merge-helper (cdr loi1) loi2 (cons (car loi1) acc))]
      [else (merge-helper loi1 (cdr loi2) (cons (car loi2) acc))])))

(define merge
  (lambda (loi1 loi2)
    (reverse (merge-helper loi1 loi2 '()))))