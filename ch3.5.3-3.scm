(define (display-stream s n)
  (if (and (> n 0) (not (stream-null? s)))
      (begin
        (display (stream-car s))
        (display " ")
        (display-stream (stream-cdr s) (- n 1)))))

(define ds display-stream)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s)
)


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;(display-stream integers 10)

;====== pairs ======
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
;(display-stream (pairs integers integers) 30)

(define (zip s t)
  (cons-stream (list (stream-car s) (stream-car t))
	       (zip (stream-cdr s) (stream-cdr t)))
)

(define (zip-with-indices s)
  (zip s integers)
)

(define (take s n)
  (if (= n 0)
      '()
      (cons-stream (stream-car s)
		   (take (stream-cdr s) (- n 1))))
)



;(ds (take integers 7) 10)

;(ds (zip-with-indices integers) 5)

(define (pairs-v s t)
  (stream-append-map (lambda (x) 
		(take (stream-map (lambda (y) (list y (cadr x)))  
				  s)
		      (car x)))
	      (zip-with-indices t))
)  

;(stream-ref (pairs integers integers) 200)

;(stream-head (stream-tail (pairs integers integers) 0) 20)


(define (pairs-all s t)
  (cons-stream (list (stream-car s) (stream-car t))
	       (interleave
		(interleave (stream-map (lambda (x) 
					  (list x (stream-car t)))
					(stream-cdr s))
			    (stream-map (lambda (y)
					  (list (stream-car s) y))
					(stream-cdr t)))
		(pairs-all (stream-cdr s) (stream-cdr t)))))



;(stream-head (pairs-all integers integers) 20)

(define (pairs-reasoner s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))

;(stream-head (pairs-reasoner integers integers) 10)

;==== ex 3.69 ====
(define (tripples-ints)
  (stream-append-map (lambda (p) 
		       (stream-map (lambda (i) (cons i p))
			    (take integers (car p)))
		       )
		     (pairs integers integers))
)


;(stream-head (tripples-ints) 20)

(define (tripples s t u)
  (interleave
   (stream-map (lambda (p) (cons (stream-car s) p)) 
			  (pairs t u))
   (tripples (stream-cdr s) (stream-cdr t) (stream-cdr u)))
)

(stream-ref (tripples integers integers integers) 10)



























