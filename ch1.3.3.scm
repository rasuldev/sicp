(define (search f neg pos)
  (let ((mid (average neg pos)))
    (if (close-enough? neg pos)
	mid
	(let ((f-mid (f mid)))	
	  (cond ((negative? f-mid) (search f mid pos))
		((positive? f-mid) (search f neg mid))
		(else mid)
	  )
	)
    )
  )
)

(define (average a b)
  (/ (+ a b) 2.0)
)

(define (close-enough? a b)
  (< (abs (- a b)) 0.001)
)


(search sin 4 2)

(define (half-interval f a b)
  (let ((f-a (f a))
	(f-b (f b)))
    (cond ((and (positive? f-a) (negative? f-b))
	   (search f b a))
	  ((and (positive? f-b) (negative? f-a))
	   (search f a b))
	  (else (error "Values are not of opposite sign" a b))
    )
  )
)

(half-interval sin 2 4)



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


(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0 )
