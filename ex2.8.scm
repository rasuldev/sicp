(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (let ((a (car interval))
	(b (cdr interval)))
    (min a b)
  )
)

(define (upper-bound interval)
  (let ((a (car interval))
	(b (cdr interval)))
    (max a b)
  )
)

(define (sub-interval x y)
  (make-interval 
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))
  )
)

