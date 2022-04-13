#lang racket
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
               [(null? ls) '()]
               [(eq? (car ls) '0) (begin
                           (last-non-zero (cdr ls))
                           (k (cdr ls)))]
               [else (cons (car ls) (last-non-zero (cdr ls)))]
  	       ))))
	(last-non-zero ls)))))
(last-non-zero '(0))

(last-non-zero '(1 2 3 0 4 5))

(last-non-zero '(1 0 2 3 0 4 5))

(last-non-zero '(1 2 3 4 5))


