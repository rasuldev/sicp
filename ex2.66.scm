(define (lookup given-key tree)
  (cond ((null? tree) false)
	((equal? given-key (key (entry tree))) (entry tree))
	((< given-key (entry tree)) (lookup given-key (right-branch tree)))
	(else (lookup given-key (left-branch tree)))
  )
)


(define nil '())
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define tree1 (make-tree 7 
			 (make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil))
			 (make-tree 9 nil (make-tree 11 nil nil))
	      )
)

(define (key entry) entry)

(lookup 6 tree1)