(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s)
)


(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess) (sqrt-improve guess x))
			     guesses)))
  guesses)

(define (average x y)
  (/ (+ x y) 2.0)
)

(define (sqrt-improve guess x)
  (average guess (/ x guess)) 
)

(define (display-stream s n)
  (if (and (> n 0) (not (stream-null? s)))
      (begin
        (display (stream-car s))
        (display " ")
        (display-stream (stream-cdr s) (- n 1)))))


;(display-stream (sqrt-stream 4) 4)


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))

(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (partial-sums s) (stream-cdr s))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1
			      (add-streams ones integers)))

(display-stream (partial-sums integers) 10)