(define (double f)
  (lambda (x)
    (f (f x))
  )
)

((double (lambda (x) (+ x 1))) 5)

(define (inc x) (+ x 1))

(((double (double double)) inc) 5)