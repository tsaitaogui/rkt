#lang racket
;1
(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr (lambda (n)
	            (if (eq? n 0)
                        ls
                        (cdr (nth-cdr (- n 1)))
                    ))])
      (car (nth-cdr n)))))

;2
(define union
  (lambda (l1 l2)
    (define foo
      (lambda (ls)
        (cond
          [(null? ls) '()]
          [else (if (memv (car ls) (cdr ls))
            (foo (cdr ls))
            (cons (car ls) (foo (cdr ls)))
        )]
        )
    )
   )
    (foo (append l1 l2)
    )))

;3
(define stretch
  (lambda (pred x)
    (lambda (data)
      (if (list? data)
          (if (or (pred (car data))
                  (eq? x (car data)
                  ))
              (cons (car data) (stretch (cdr data)))
              (stretch (cdr data))
          )
          (or (pred data)
              (eq? x data))
      )
    )))

;4
(define walk-symbol
  (lambda (sym ls)
    (let ([assed (assv sym ls)])
    (if assed
         (if (symbol? (cdr assed))
              (walk-symbol (cdr assed) ls)
              (cdr (assv sym ls))
         )
         sym
    ))))

;5
(define lambda-exp?
  (λ (E)
    (letrec
      ([p
        (λ (e)
          (match e
            [(? symbol?) #t]
            [`(lambda (,x) ,body) (and (symbol? x)
                                       (p body))]
            [`(,rator ,rand) (and (p rator) (p rand))]
            [else #f]))])
      (p E))))