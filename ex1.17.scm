(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (mul a b)
  (cond ((= b 0) 0)
	((even? b) (mul (double a) (halve b)))
	(else (+ a (mul a (- b 1))))
  )
)


(mul 0 5)


(define (mul-i a b)
  (if (< a b)
      (mul b a)
      (cond ((= b 0) 0)
	    ((even? b) (mul (double a) (halve b)))
	    (else (+ a (mul a (- b 1))))
      )
  )
)

(mul-i 7 5)