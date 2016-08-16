(define (square x) (* x x))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)

(define (cube-root-iter guess x)
  (let ((newguess (improve guess x))) 
    (if (< (abs (- 1 (/ newguess guess))) 0.001) 
	newguess
	(cube-root-iter newguess x))
  )
)

(define (cube-root x)
  (cube-root-iter 1.0 x)
)

(cube-root 8)

