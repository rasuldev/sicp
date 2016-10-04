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

(define (ave-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)
  )
)

(define (compose f g)
  (lambda (x)
    (f (g x))
  )
)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))
  )
)

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))
  )
)


(define (nth-root n x guess)
  (fixed-point 
   ((repeated ave-damp (- n 1)) (lambda (y) (/ x (pow y (- n 1)))))
   guess
  )
)

(nth-root 3 8 3.0)
