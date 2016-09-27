(define tolerance 0.00001)

(define (fixed-point f init)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance)
  )
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? next guess) 
	  next
	  (try next)
      )
    )
  )
  (try init)
)

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)




