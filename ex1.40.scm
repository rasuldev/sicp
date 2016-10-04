(define tolerance 0.00001)

(define (fixed-point f init)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance)
  )
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess) 
	  next
	  (try next)
      )
    )
  )
  (try init)
)

(define dx 0.00001)

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) dx)
  )
)

(define (newton-transform g)
  (lambda (x) 
    (- x (/ (g x) ((deriv g) x)))
  )
)

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess)
)

((deriv (lambda (x) (* x x))) 2)

(define (sqrt x)
  (newtons-method (lambda (y) (- (* y y) x))
		  1.0)
)

(sqrt 9)

(define (cubic a b c)
  (lambda (x) 
    (+ (* x x x) (* a (* x x)) (* b x) c)
  )
)

(define (cubic-zero a b c init)
  (newtons-method (cubic a b c) init)
)

(cubic-zero 0 -1 0 0.1)








