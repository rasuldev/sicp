(define (iter-improve close-enough? improve)
  (define (iter guess)
    (let ((next (improve guess)))    
      (if (close-enough? next guess)
	  next
	  (iter next)
      )
    )
  )
  (lambda (guess) (iter guess))
)


(define (fixed-point f init)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance)
  )
  ((iter-improve close-enough? f) init)
)

