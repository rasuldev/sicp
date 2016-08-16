(define (average x y)
  (/ (+ x y) 2 ))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x) 
      guess 
      (sqrt-iter (improve guess x) 
		 x)))

(define (sqrt-iter-i guess x)
  (let ((newguess (improve guess x))) 
    (if (< (abs (- 1 (/ newguess guess))) 0.001)
	newguess
	(sqrt-iter-i newguess x)
     )
  )  
)

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-i x)
  (sqrt-iter-i 1.0 x))


;(trace good-enough?)
;(sqrt 2)



