(define (deep-reverse items)
  (define (iter lst rvs)
    (if (null? lst) 
	rvs
	(iter (cdr lst) 
	      (cons (let ((x (car lst))) 
		      (if (pair? x)
			  (deep-reverse x)
			  x
		      )
		    )
		    rvs))
    )
  )
  (iter items '())
)

(define (deep-reverse2 items)
  (define (iter left right)
    (cond ((null? left) right)
	  ((not (pair? left)) left)
	  (else (iter (cdr left) 
		      (cons (deep-reverse2 (car left)) right)))
    )
  )
  (iter items ())
)

(define x (list (list 1 2) (list 3 4)))
(deep-reverse2 x)



