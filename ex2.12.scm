(define (make-interval a b) 
  (if (< a b)
      (cons a b)
      (cons b a))
)

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
  (/ (* 100 (width x)) (center x))
)


