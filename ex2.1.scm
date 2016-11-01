(define (gcd a b)
	(if (= b 0) a
		    (gcd b (remainder a b))
	)
)

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (if (< d 0)
      (make-rat (* -1 n) (* -1 d))
      (let ((g (gcd n d)))
	(cons (/ n g) (/ d g)))
  )
)



(print-rat (make-rat -2 -3))
