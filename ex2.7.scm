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

(lower-bound (make-interval 2 3))
(lower-bound (make-interval 3 2))

