(define (smallest-divisor n)
  (find-divisor n 2)  
)

(define (find-divisor n d)
  (cond ((> (square d) n) n)
	((divides? n d) d)
	(else (find-divisor n (next d)))
  ) 
)

(define (next n)
  (if (= n 2)
      3
      (+ n 2)
  )
)

(define (divides? n d)
  (= (remainder n d) 0))

(define (prime? n)
  (= (smallest-divisor n) n)
)


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
  (if (prime? n) 
      (report-prime (- (runtime) start-time))
      #f)  
)

(define (report-prime elapsed)
  (display " *** ")
  (display elapsed)
  #t
)



(timed-prime-test 1000003)






