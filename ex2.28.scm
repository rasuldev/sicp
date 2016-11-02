(define (fringe items)
  (if (null? items)
      ()
      (let ((head (car items)))
	(if (pair? head)
	    (append (fringe (car items)) (fringe (cdr items)))
	    (cons head (fringe (cdr items)))
	)
      )      
  )
)


(define x (list (list 1 2) (list 3 4)))
(fringe x)

(fringe (list x x))