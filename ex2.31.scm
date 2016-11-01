(define (tree-map proc tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (proc tree))
	(else (cons (tree-map proc (car tree))
		    (tree-map proc (cdr tree)))
	)
  )
)

(define (tree-map2 proc tree)
  (map (lambda (sub-tree) 
	 (if (pair? sub-tree)
	     (tree-map2 proc sub-tree)
	     (proc sub-tree))
       )
       tree
  )
)

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


