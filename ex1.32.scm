(define (acc combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(acc combiner null-value term (next a) next b))
  )
)

(define (acc-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))
    )
  )
  (iter a null-value)
)

(define (sum term a next b)
  (define (add x y) (+ x y))
  (acc-iter add 0 term a next b)
)

(define (inc x) (+ x 1))
(define (identity x) x)

(sum identity 1 inc 10)

(define (prod term a next b)
  (define (mul x y) (* x y))
  (acc-iter mul 1 term a next b)
)

(prod identity 1 inc 5)

