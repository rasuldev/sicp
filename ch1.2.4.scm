(define (expt b n)
  (if (= n 0) 
      1
      (* b (expt b (- n 1))))
)

(expt 2 10)

(define (expt-i b n)
  (define (iter acc n)
    (if (= n 0)
	acc
	(iter (* acc b) (- n 1))))

  (iter 1 n)
)

(expt-i 2 4)








