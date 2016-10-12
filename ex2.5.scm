(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1))))
)


(define (mcons a b)
  (* (pow 2 a) (pow 3 b))
)

(define (car z)
  (define (iter x count)
    (if (= (remainder x 2) 0)
	(iter (/ x 2) (+ count 1))
	count
    )
  )
  (iter z 0)
)

(define (cdr z)
  (define (iter x count)
    (if (= (remainder x 3) 0)
	(iter (/ x 3) (+ count 1))
	count
    )
  )
  (iter z 0)
)

(car (mcons 5 2))
(cdr (mcons 5 2))
