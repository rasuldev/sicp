(define zero 
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
	(lambda (f) (lambda (x) (f ((n f) x)))))


(define (inc x) (+ x 1))
(((add-1 zero) inc) 0)

((zero inc) 0)

(define one (lambda (f) (lambda (x) (f x))))

((one inc) 0)

(define two (lambda (f) (lambda (x) (f (f x)))))
((two inc) 0)


(define (add n m)
	(lambda (f) (lambda (x) ((m f) ((n f) x))))
)

(((add one two) inc) 0)