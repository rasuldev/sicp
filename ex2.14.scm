(define (make-interval a b) 
  (if (< a b)
      (cons a b)
      (cons b a))
)

(define (lower-bound interval)
  (car interval)
)

(define (upper-bound interval)
  (cdr interval)
)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (display "error: division by zero")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))
  )
)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y)))
)

(define (print-interval x)
  (newline)
  (display "(")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display ")")
)


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))




(define r1 (make-interval 99 101))
(define r2 (make-interval 99 101))

(print-interval (par1 r1 r2))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c percent)
  (make-center-width c (* c (/ percent 100.0)))
)
(define (percent x)
  (/ (* 100.0 (width x)) (center x))
)




(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))




(define r1 (make-interval 999 1001))
(define r2 (make-interval 99 101))

(print-interval (par1 r1 r2))
(print-interval (par2 r1 r2))


(print-interval (mul-interval r1 (div-interval r1 r1)))

(percent (add-interval r1 r2))
(percent r1)
(percent r2)









