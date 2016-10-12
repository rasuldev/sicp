(define (make-segment start end)
  (cons start end)
)

(define (start-segment segment)
  (car segment)
)

(define (end-segment segment)
  (cdr segment)
)

(define (length-segment segment)
  (let ((s (start-segment segment))
	(e (end-segment segment)))    
    (sqrt (+ (square (- (x-point s) (x-point e)))
	     (square (- (y-point s) (y-point e)))))
  )
)

(define (make-point x y)
  (cons x y)
)

(define (x-point point)
  (car point)
)

(define (y-point point)
  (cdr point)
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)

(define (rectangle p1 p2 p3)
  (cons (cons p1 p2) p3)
)

(define (width-rect rect)
  (let ((p3 (cdr rect))
	(p2 (cdr (car rect)))
	(p1 (car (car rect)))
       )
    (min ()
	 ()
	 ()
     )

  )
)

(define (length-rect rect)
  
)

(define (perimeter rect)
  (* 2 (+ (width-rect rect) (length-rect rect)))
)

(define (area rect)
  (* (width-rect rect) (length-rect rect))
)

