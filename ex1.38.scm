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


(define (d-euler i)
  (if (not (= (remainder (+ i 1) 3) 0))
      1
      (let ((j (/ (+ i 1) 3)))
	(* 2 j)
      )
  )
)

(define (for f start end)
  (if (<= start end)
      (begin 
	(display (f start))
	(newline)
	(for f (+ start 1) end)
      )
      (display "END"))
)



(cont-frac (lambda (i) 1.0) d-euler 100)





