(define (smallest-divisor n)
  (find-divisor n 2)  
)

(define (find-divisor n d)
  (cond ((> (square d) n) n)
	((divides? n d) d)
	(else (find-divisor n (+ d 1)))
  )
)

(define (divides? n d)
  (= (remainder n d) 0))

(define (prime? n)
  (= (smallest-divisor n) n)
)



(smallest-divisor 19999)


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




(timed-prime-test 199)

(define (search-for-primes start end)
  (define (iter-check-for n)
    (timed-prime-test n)
    (if (< n end) 
	(iter-check-for (+ n 2)))
  )
  
  (iter-check-for 
   (if (not (even? start))
       start
       (+ start 1)))
)


(define (three-primes from)
  (define (iter n count)
    (if (< count 3)
	(if (timed-prime-test n)
	    (iter (+ n 2) (+ count 1))
	    (iter (+ n 2) count)
	)
    )	
  )
  (iter (if (even? from) 
	    (+ from 1) 
	    from) 
	0)
)






