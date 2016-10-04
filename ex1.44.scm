(define dx 0.1)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)
  )
)

((smooth (lambda (x) (* x x))) 1)


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


(define (n-smooth f n)
  ((repeated smooth n) f)
)

((n-smooth (lambda (x) (* x x)) 2) 1)