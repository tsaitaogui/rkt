(define x #f)
(define p
  (lambda ()
    (call/cc (lambda (k)
               (set! x k) 4)
             ;5 if there is a return value here, the result will be different
             )))

(cons (p) 2)

(x 1)