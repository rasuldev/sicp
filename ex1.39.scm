(define (tan-cf x k)
  (define sq-x (* x x))
  (define (step j)
    (if (= j k)
	0
	(let ((d (+ (* 2 j) 1)))
	  (/ sq-x (- d (step (+ j 1))))
	)
    )
  )

  (/ x 
     (- 1 (step 1)))
)


(tan-cf (/ 3.14 4) 155)