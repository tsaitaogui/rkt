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

;6
(define var-occurs?
  (lambda (sym lcExp)
    (match lcExp
      [(? symbol?) (eq? sym lcExp)]
      [`(lambda (,x) ,body) (and (not (eq? sym x))
                                 (var-occurs? sym body))]
      [`(,rator ,rand) (or (var-occurs? sym rator)
                            (var-occurs? sym rand))]
      [else #f]
    )
  ))

;7
(define vars
  (lambda (lcExp  [result '()])
    (match lcExp
      [(? symbol?) (set! result (cons lcExp result)) result]
      [`(lambda (,x) ,body) (vars body result)]
      [`(,rator ,rand) (append (vars rator result)
                               (vars rand result))])
  ))


    