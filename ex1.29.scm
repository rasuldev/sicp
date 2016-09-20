(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))
  )
)

(define (inc x) (+ x 1))
(define (identity x) x)
(define (cube x) (* x x x))

(sum identity 1 inc 10)

(define (simpson f a b n)
  (define (step) (/ (- b a) n))
  (define (f-mul k)
    (* (if (even? k) 4 2)
       (f (+ a (* k (step)))) 
    )
  )
  (* (/ (step) 3.0)
     (+ (sum f-mul 1 inc (- n 1))
	(f a)
	(f b)
     )
  )
)



(simpson cube 0 1 100)
