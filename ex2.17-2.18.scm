(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))
  )
)

(last-pair (list 1 2 3))

(define (reverse lst)
  (define (iter lst rvs)
    (if (null? lst)
	rvs
	(iter (cdr lst) (cons (car lst) rvs))
    )
  )
  (iter lst '())
)


(reverse (list 1 2 3))
