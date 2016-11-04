(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence))
      )
  )
)

(define (horner-eval x coeffs)
  (accumulate (lambda (coeff acc) (+ coeff (* x acc)))
	      0
	      coeffs
  )
)

(horner-eval 2 '(1 1 1))