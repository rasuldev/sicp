(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (count-leaves-acc t)
  (accumulate (lambda (h acc) 
		(if (pair? h)
		    (+ acc (count-leaves-acc h))
		    (+ acc 1)
		)
	      )
	      0
	      t
  )
)

(define (count-leaves-map t)
  (accumulate +
	      0
	      (map (lambda (x) 
		     (if (pair? x)
			 (count-leaves-map x)
			 1))
		   t
	      )
  )
)

(define x (cons (list 1 2) (list 3 4)))

(count-leaves-map (list x x))
;(count-leaves-map '(x x))