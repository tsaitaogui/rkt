#lang racket

;1
(define lex
(λ (e assoc lv)
          (match e
            [(? symbol?)
             (let ((result (find assoc e)))
               (if (symbol? result)
                   result
                   `(var ,(sub1 (- lv result)))
               ))]
            [`(lambda (,x) ,body) `(lambda ,(lex body (construct assoc x lv) (add1 lv)))]
            [`(,rator ,rand) `(,(lex rator assoc lv)
                               ,(lex rand assoc lv))]
            [else #f])))


(define find
  (λ (assoc sym)
    (cond
      [(null? assoc) sym]
      [(eq? (caar assoc) sym) (cadar assoc)]
      [else (find (cdr assoc) sym)])))


(define construct
  (λ (assoc sym lv)
    (cond
      [(null? assoc) `((,sym ,lv))]
      [(eq? (caar assoc) sym)
         (cons (cons (caar assoc) (append `(,lv) (cdar assoc) )) (cdr assoc))]
      [else (cons (car assoc) (construct (cdr assoc) sym lv))]
    )))


(lex '(lambda (a)
          (lambda (b)
	    (lambda (c)
	      (lambda (w)
	        (lambda (x)
		  (lambda (y)
		    ((lambda (a)
		       (lambda (b)
			 (lambda (c)
			   (((((a b) c) w) x) y))))
		     (lambda (w)
		       (lambda (x)
			 (lambda (y)
			   (((((a b) c) w) x) y)))))))))))          '() 0)
