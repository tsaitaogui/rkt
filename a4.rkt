#lang racket

;1
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps
             (cdr n)
             (lambda (v)
              (k (+ (car n) (* 2 v)))))]
      )))


(define empty-k
  (lambda ()
    (lambda (v) v)))

(binary-to-decimal-cps '() (empty-k))
(binary-to-decimal-cps '(1) (empty-k))
(binary-to-decimal-cps '(0 1) (empty-k))
(binary-to-decimal-cps '(1 1 0 1) (empty-k))

;2
(define star-cps
  (lambda (m k)
    (lambda (n)
      (k (* m n)))))

((star-cps 2 (empty-k)) 3)

((star-cps ((star-cps 2 (empty-k)) 3) (empty-k)) 5)

;3
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (v)
                                  (k (* (car ls) v))))
       #;(* (car ls) (times (cdr ls)))])))

(times-cps '(1 2 3 4 5) (empty-k))

(times-cps '(1 2 3 0 3) (empty-k))