(define nil '())

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))
  )
)

(enumerate-interval 1 3)

(define (flatmap proc seq)
  (fold-right append
	      nil
	      (map proc seq))
)




(define (triples n s)
  (define (distinct-and-sum-is-s? triple)
    (let ((a (car triple)) 
	  (b (cadr triple)) 
	  (c (caddr triple)))
      (and (not (or (= a b) (= a c) (= b c)))
	   (= (+ a b c) s))
    )
  )
  
  (filter distinct-and-sum-is-s?
	  (flatmap (lambda (i) 
		     (flatmap (lambda (j) 
				(map (lambda (k) 
				       (list i j k)
				       )
				     (enumerate-interval 1 n)
				     )
				)
			      (enumerate-interval 1 n)
			      )
		   )
		   (enumerate-interval 1 n)
		   )
 )
)

(triples 3 6)


