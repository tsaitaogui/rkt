#lang racket
(define countdown1
  (lambda (x)
    (let ((q 0))
(if (zero? x)
                    `(,q)
                     (cons x (countdown1 (- x 1)))
                 )
      )
                 ))

(define countdown
  (lambda (n)
    (cond
     ((zero? n) '(0))
     (else (cons n (countdown (sub1 n)))))))

;(countdown1 5)

(define foo
  (lambda (a b c)
    (if (eqv? (car c) a)
        (if (eqv? (cdr c) '())
            (list a b)
            (cons a (cons b (foo a b (cdr c))))
         )
        (if (eqv? (cdr c) '())
            (car c)
            (cons (car c) (foo a b (cdr c)))    
            )
        
     )))

;(foo 'x 'y '(x z z x y x))


(define remv-1st
  (lambda (symbol list)
    (if (eqv? list '())
        '()
        (if (eqv? symbol (car list))
        (cdr list)
        (cons (car list) (remv-1st symbol (cdr list)))        
        )
        )
  ))
;(remv-1st 'x '(x y z x))
;(remv-1st 'y '(x y z y x))
;(remv-1st 'z '(a b c))
;(insertR 'x 'y '(x z z x y x)) ;(x y z z x y y x y)

(define add1
  (lambda (x)
    (+ x 1)))

(define mymap
  (lambda (fn ls)
    (if (eqv? ls '())
        '()
    (cons (fn (car ls)) (mymap fn (cdr ls))))))


; (mymap add1 '(1 2 3 4))

(define zip
  (lambda (ls1 ls2)
    (cond
      [(eq? ls2 '()) '()]
      [(eq? ls1 '()) '()]
      [else (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)) )]
    )))

(define my-even?
  (lambda (num)
    (cond
      [(eq? (modulo num 2) 0) #t]
      [else #f]
      )))

(define my-filter
  (lambda (pred ls)
    (cond
      [(eq? ls '()) '()]
      [(if (pred (car ls))
       (cons (car ls) (my-filter pred (cdr ls)))
       '())
       ]
      )))



(define list-index-ofv
  (let ([i 0])
    (lambda (elem ls)
      (cond
        [(eq? ls '()) 'Bad]
        [(eq? elem (car ls)) i]
        [else (begin
               (set! i (+ i 1))
               (list-index-ofv elem (cdr ls)))]
        )
      ))
  )

(define my-append
  (lambda (ls1 ls2)
      [if (eq? ls1 '())
             [if (eq? ls2 '())
                 '()
                 (cons (car ls2) (my-append (cdr ls2)))    
                 ]
             (cons (car ls1) (my-append (cdr ls1)))
             ]
      
    ))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))
  

;(define my-reverse
;  (let ([foo (lambda (ls) (
;                           (+ 1 1)
;                           ))]
;        [bar (lambda (ls) (
;                           (if (atom? ls)
;                               (list ls)
;                               (list (bar (cdr ls)) (list (car ls)))
;                               )
;                           ))])
;    (lambda (ls) (bar ls))
;  ))

(define cat
  (lambda (ls elem)
    (if (eq? (length ls) 1)
        (list (car ls) elem)
    (cons (car ls) (cat (cdr ls) elem)))
        ))

(define my-reverse
  (lambda (ls)
    (cond
      [(eq? (length ls) 1) (car ls)]
      [(eq? (length ls) 2) (list (cadr ls) (car ls))]
      [else (cat (my-reverse (cdr ls)) (car ls))]
    )))
;(let* ([x (list "Burroughs")]
;         [y (cons "Rice" x)]
;         [z (cons "Edgar" y)])
;    (list x y z))