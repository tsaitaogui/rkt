#lang racket

(define assoc '())
(define ss 0)
(define lc
(λ (e)
          (match e
            [(? symbol?) (display e) (newline)]
            [`(lambda (,x) ,body) (begin
                                    (set! ss (add1 ss))
                                    (set! assoc (cons x assoc))
                                       (lc body))]
            [`(,rator ,rand) (begin
                               (lc rator)
                               (lc rand))]
            [else #f])))

(define atom?
  (λ (x)
    (not (pair? x)
         (null? x))))
(define find
  (λ (assoc sym lv)
    (cond
      [(null? assoc) '()]
      [(eq? (caar assoc) sym)
         (cons (cons (caar assoc) (append (cdar assoc) `(,lv))) (cdr assoc))]
      [else (cons (car assoc) (find (cdr assoc) sym lv))]
    )))


(lc '(lambda (a)
          (lambda (b)
            (lambda (c)
              (lambda (a)
                (lambda (b)
                  (lambda (d)
                    (lambda (a)
                      (lambda (e)
                        (((((a b) c) d) e) a))))))))))


(define foo
  (λ (l sym lv)
    (if (eq? (caar l) sym)
        (cons (caar l) (- (car (cadar l)) lv))
        (foo (cdr l) sym (add1 lv))
    )))

(define bar
  (λ (x)
    `(,@x 1)))

(bar '(+ 1 2))
(bar `(+ 1 2))
;(bar `(+ 1 2))

    

;(foo '((x (3 2 1))) 'x 0)
(find '((x 1 2 3) (y 1 2 4) (z 2 3)) 'y 5)