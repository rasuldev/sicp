(define nil '())

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))
  )
)

(enumerate-interval 1 3)

(define (flatmap proc seq)
  (fold-right append
	      nil
	      (map proc seq))
)

(define (unique-pairs n)
  (flatmap (lambda (i) 
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1)))
	   )
	   (enumerate-interval 1 n)
  )
)


(unique-pairs 5)

(define (prime? n)
  (define (iter k)
    (cond ((> (square k) n) true)
	  ((= (remainder n k) 0) false)
	  (else (iter (+ k 1)))
    )
  )
  (iter 2)
)

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))
  )
)

(prime-sum-pairs 5)