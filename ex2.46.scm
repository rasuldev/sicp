(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
	     (+ (ycor-vect u) (ycor-vect v)))
)

(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
	     (- (ycor-vect u) (ycor-vect v)))
)


(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v)))
)




