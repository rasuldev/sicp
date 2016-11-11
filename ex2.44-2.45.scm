(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter 
	       (beside smaller smaller)
	)
      )
  )
)


(define (split bigOp smallOp)
  (define (splitter painter n)
    (if (= n 0)
	painter
	(let ((smaller (splitter painter (- n 1)))
	  (bigOp painter
		 (smallOp smaller smaller))
	  )    
	)
    )
  )
  splitter
)



