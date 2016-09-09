(define (expt b n)
  (if (= n 0) 
      1
      (* b (expt b (- n 1))))
)

(define (expt-i b n)
  (define (iter acc n)
    (if (= n 0)
	acc
	(iter (* acc b) (- n 1))))

  (iter 1 n)
)

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (fast-expt (square b) (/ n 2)))
	(else (* b (fast-expt b (- n 1))))   
   )
)

