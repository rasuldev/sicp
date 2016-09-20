(define (filtered-acc filter? combiner null-value term a next b)
  (define (f-acc a)
    (filtered-acc filter? combiner null-value term a next b)
  )
  (cond ((> a b) null-value)
	((filter? a) (combiner (term a) (f-acc (next a))))
	(else (f-acc (next a)))
  )
)


(define (add x y) (+ x y))
(define (mul x y) (* x y))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

(filtered-acc even? add 0 identity 1 inc 10)


(define (smallest-divisor n)
  (find-divisor n 2)  
)

(define (find-divisor n d)
  (cond ((> (square d) n) n)
	((divides? n d) d)
	(else (find-divisor n (next d)))
  ) 
)

(define (next n)
  (if (= n 2)
      3
      (+ n 2)
  )
)

(define (divides? n d)
  (= (remainder n d) 0))

(define (prime? n)
  (= (smallest-divisor n) n)
)



(define (sum-prime a b)
  (filtered-acc prime? add 0 square a inc b)
)


(sum-prime 1 10)

(define (rel-prime? a b)
  true
)

(define (gcd a b)
	(if (= b 0) a
		    (gcd b (remainder a b))
	)
)


(define (product-rel-prime n)
  (define (prime-to? m)
    (= 1 (gcd m n))
  )

  (filtered-acc prime-to? mul 1 identity 2 inc (- n 1))
)

(product-rel-prime 5)












