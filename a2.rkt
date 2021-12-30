#lang racket
(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr (lambda (n)
	            (if (eq? n 0)
                        ls
                        (cdr (nth-cdr (- n 1)))
                    ))])
      (car (nth-cdr n)))))

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

(define walk-symbol
  (lambda (sym ls)
    (if (assv sym ls)
         (if (symbol? (cdr (assv sym ls)))
              (walk-symbol (cdr (assv sym ls)) ls)
              (cdr (assv sym ls))
         )
         sym
    )))