#lang racket
 (require racket/trace)
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
(trace times-cps)
(times-cps '(1 2 3 4 5) (empty-k))

(times-cps '(1 2 3 0 3) (empty-k))

;4
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (cond
         [(equal? (car ls)
                  (remv-first-9*-cps (car ls) (lambda (v)                                         
                                                v)))
          (remv-first-9*-cps (cdr ls) (lambda (v)
                                        (k (cons (car ls) v))))]
         [else (remv-first-9*-cps (car ls) (lambda (v)
                                               (k (cons v (cdr ls)))))])]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (v)
                                      (k (cons (car ls) v))))])))
(display "----------------\n")
;(trace remv-first-9*-cps)
(remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
(remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
(remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))

;5
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
      (k (cons-cell-count-cps (car ls) (lambda (v)
            (add1 (+ v (cons-cell-count-cps (cdr ls) (lambda (v) v)))))))]
      [else (k 0)])))

(cons-cell-count-cps '(1 2 3 4) (empty-k))
(cons-cell-count-cps '(1 2 (3 (4) 5) 4 ()) (empty-k))

(define log
  (lambda (v)
    (display v)
    (newline)))