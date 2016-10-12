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

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (display "error: division by zero")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))
  )
)


(define x (make-interval 2 3))
(define y (make-interval -1 1))

(div-interval x y)