(define (same-parity x . lst)
  (define (iter cand)
    (if (null? cand)
	'()
	(let ((y (car cand)))
	  (if (= (remainder x 2) (remainder y 2)) 
	      (cons y (iter (cdr cand)))
	      (iter (cdr cand))
          )
        ) 
    )
  )

  (cons x (iter lst))
)


(same-parity 2 3 4 5 6 7)


