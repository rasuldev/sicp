(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))
  )
)

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1)
  )
  
  (try-it (+ 1 (random (- n 1))))
)




