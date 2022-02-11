#lang racket


(define lc
(λ (e assoc lv)
          (match e
            [(? symbol?) `(var ,(- lv (find assoc e)))]
            [`(lambda (,x) ,body) `(lambda ,(lc body (construct assoc x lv) (add1 lv)))]
            [`(,rator ,rand) `(,(lc rator assoc lv)
                               ,(lc rand assoc lv))]
            [else #f])))


(define find
  (λ (assoc sym)
    (cond
      [(null? assoc) '()]
      [(eq? (caar assoc) sym) (cadar assoc)]
      [else (find (cdr assoc) sym)])))


(define construct
  (λ (assoc sym lv)
    (cond
      [(null? assoc) `((,sym ,lv))]
      [(eq? (caar assoc) sym)
         (cons (cons (caar assoc) (append `(,(add1 lv)) (cdar assoc) )) (cdr assoc))]
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
                        (((((a b) c) d) e) a)))))))))  '() 0)

