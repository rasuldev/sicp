(define (cont-frac n d k)
  (define (step j)
    (if (= j k)
	(/ (n j) (d j))
	(/ (n j) (+ (d j) (step (+ j 1))))
    )
  )
  (step 1)
)

(define (cont-frac-iter n d k)
  (define (iter result j)
    (if (= j 0)
	result
	(iter (/ (n j) (+ result (d j)))  (- j 1))
    )
  )
  (iter 0.0 k)
)


(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   k)