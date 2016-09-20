(define (prod term a next b)
  (if (> a b)
      1
      (* (term a) (prod term (next a) next b))
  )
)

(define (product term a next b)
  (define (iter a result) 
    (if (> a b)
	result
	(iter (next a) (* result (term a))))
  )
  (iter a 1)
)

(define (inc x) (+ x 1))
(define (identity x) x)

(define (fact n)
  (product identity 1 inc n)
)

(fact 5)

(define (pi-wallis n)
  (define (term n) 
    (if (even? n)
	(/ n (+ n 1.0))
	(/ (+ n 1.0) n)
    )
  )
  (* 4 (product term 2 inc n))
)

(pi-wallis 1000)
