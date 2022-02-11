#lang racket

(define assoc '())
(define ss 0)
(define lc
(λ (e assoc lv)
          (match e
            [(? symbol?) (display assoc)
                         (newline)]
            [`(lambda (,x) ,body) (begin
                                    (let ((sk (find assoc x)))
                                      (if (null? sk)
                                       (lc body `(,assoc (,x ,lv)) (add1 lv))
                                       (lc body (construct assoc x (add1 lv)) (add1 lv))
                                       )))]
            [`(,rator ,rand) (begin
                               (lc rator assoc (add1 lv))
                               (lc rand assoc (add1 lv)))]
            [else #f])))

(define atom?
  (λ (x)
    (not (pair? x)
         (null? x))))

(define find
  (λ (assoc sym)
    (cond
      [(null? assoc) '()]
      [(eq? (car assoc) sym) (car assoc)]
      [else (find (cdr assoc) sym)])))

(define construct
  (λ (assoc sym lv)
    (cond
      [(null? assoc) (begin
                       (display "ssss"))]
      [(eq? (caar assoc) sym)
         (cons (cons (caar assoc) (append (cdar assoc) `(,lv))) (cdr assoc))]
      [else (cons (car assoc) (construct (cdr assoc) sym lv))]
    )))


(lc '(lambda (a)
          (lambda (b)
            (lambda (c)
              (lambda (a)
                (lambda (b)
                  (lambda (d)
                    (lambda (a)
                      (lambda (e)
                        (((((a b) c) d) e) a))))))))) '() 0)


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
(construct '((x 1 2 3) (y 1 2 4) (z 2 3)) 'y 5)