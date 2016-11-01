(define (make-interval a b) 
  (if (< a b)
      (cons a b)
      (cons b a))
)

(define (lower-bound interval)
  (car interval)
)

(define (upper-bound interval)
  (cdr interval)
)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (mul-interval-i x y)
  (let ((x1 (lower-bound x))
	(x2 (upper-bound x))
	(y1 (lower-bound y))
	(y2 (upper-bound y)))
    (cond ((and (>= x1 0) (>= y1 0)) (make-interval (* x1 y1) (* x2 y2)))
	  ((and (<= x2 0) (<= y2 0)) (make-interval (* x2 y2) (* x1 y1)))

	  ((and (< x2 0) (>= y1 0)) (make-interval (* x1 y2) (* x2 y1)))
	  ((and (< y2 0) (>= x1 0)) (make-interval (* x2 y1) (* x1 y2)))

	  ((and (< x1 0) (>= x2 0) (> y1 0)) (make-interval (* x1 y2) (* x2 y2)))
	  ((and (< y1 0) (>= y2 0) (> x1 0)) (make-interval (* x2 y1) (* x2 y2)))

	  ((and (< x1 0) (>= x2 0) (< y2 0)) (make-interval (* x2 y1) (* x1 y1)))
	  ((and (< y1 0) (>= y2 0) (< x2 0)) (make-interval (* x1 y2) (* x1 y1)))

	  (else (make-interval (min (* x1 y2) (* x2 y1))
			       (max (* x1 y1) (* x2 y2))
		)
	  )
    )
  )
)

(define (print-interval x)
  (newline)
  (display "(")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display ")")
)

(define x (make-interval -5 2))
(define y (make-interval 3 7))
(define z1 (mul-interval x y))
(define z2 (mul-interval-i x y))

(print-interval z1)
(print-interval z2)







