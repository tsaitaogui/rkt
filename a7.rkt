#lang racket
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
               [(null? ls) (k)]
               [(eq? (car ls) '0) (begin
                           (set! k (lambda () (cdr ls))) (last-non-zero (cdr ls)))]
               [else (begin
                           (set! k (lambda () ls)) (last-non-zero (cdr ls)))
                       ]
  	       ))))
	(last-non-zero ls)))))
(last-non-zero '(0))

(last-non-zero '(1 2 3 0 4 5))

(last-non-zero '(1 0 2 3 0 4 5))

(last-non-zero '(1 2 3 4 5))


