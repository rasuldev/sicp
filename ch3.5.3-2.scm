(define odd (cons-stream 1 
			 (stream-map (lambda (x) (+ x 2)) 
				     odd)))

(define (display-stream s n)
  (if (and (> n 0) (not (stream-null? s)))
      (begin
        (display (stream-car s))
        (display " ")
        (display-stream (stream-cdr s) (- n 1)))))

;(display-stream odd 5)


(define (stream-limit s tol)
  (cdr (stream-car
	(stream-filter (lambda (x) (< (car x) tol))
		       (stream-map (lambda (x y) 
				     (cons (abs (- x y)) y)) 
				   s (stream-cdr s)))))
)


(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln2-summands (+ n 1))))
)
			 

(display-stream (ln2-summands 1) 10)


;================================

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))
