(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))


(define (rand)
  (random 1000)
)

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1)
		 (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1)
		 trials-passed))))
  (iter trials 0))


(define (estimate-integral x1 x2 y1 y2 P trials)
  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))


  (* 1.0 (- x2 x1) (- y2 y1)
     (monte-carlo trials (lambda ()
			   (let ((x (random-in-range x1 x2))
				 (y (random-in-range y1 y2)))
				 (P x y))))
  )
)

(define (P x y)
  (<= (+ (* x x) (* y y)) 1)
)


(estimate-integral -1 1 -1 1 P 10000)

