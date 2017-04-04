;=========Tree========
(define (make-tree root-entry key-selector)

	(define (make-tree entry left right)
  		(list entry left right))
	(define (entry tree) (car tree))
	(define (left-branch tree) (cadr tree))
	(define (right-branch tree) (caddr tree))
	(define (key tree) (key-selector (entry tree)))

	(let ((local-tree (make-tree root-entry '() '())))
		(define (adjoin entry tree)
		  (cond ((null? tree) (make-node entry '() '()))
				((= (key-selector entry) (key tree)) tree)
				((< (key-selector entry) (key tree))
			 		(make-tree (entry tree)
				    	(adjoin entry (left-branch tree))
				    	(right-branch tree)))
				((> (key-selector entry) (key tree))
			 		(make-tree (entry tree)
				    	(left-branch tree)
				    	(adjoin node (right-branch tree))))))

		(define (lookup entry-key tree)
			(cond ((null? tree) false)
				((= entry-key (key tree)) tree)
				((< entry-key (key tree)) (lookup entry-key (left-branch tree)))
				(else (lookup entry-key (right-branch tree)))))

		(define (dispatch m)
			(cond ((m eq? 'adjoin) (lambda (entry) (set! local-tree (adjoin entry local-tree))))
					((m eq? 'lookup) (lambda (entry-key) (lookup entry-key local-tree)))
					((m eq? 'entry) (entry local-tree))
				  	(else (error "MAKE-TREE no such command" m))))
	)
	
)

;=========Balancing tree procedure========
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree
				 (cdr non-left-elts)
				 right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree
		       this-entry left-tree right-tree)
		      remaining-elts))))))))


;===========Table============
(define (make-bin-table)
	(define (make-table key)
		(make-tree (make-tree (cons key false)) 
			(lambda (tree) (car (tree 'entry))))
	)

	(define (get-table-value table)
		(cdr ((table 'entry) 'entry)))

   	(let ((local-table (make-table '*table*)))
	    (define (lookup table keys)
	    	(let ((subtable((table 'lookup) (car keys))))
	    		(if subtable
	    			(if (null? (cdr keys))
	    				(get-table-value subtable)
	    				(lookup subtable (cdr keys)))
	    			false)
	    	))

	    (define (insert! table keys value)
	    	(let ((subtable ((table 'lookup) (car keys))))
	    		(if subtable
	    			;exists
	    			(if (null? (cdr keys))
	    				(set-table-value (car keys) value)
	    				(insert! subtable (cdr keys) value))
	    			;doesn't exist
	    			(let ((new-subtable (make-table (car keys)))))
	    				((table 'adjoin) new-subtable)
	    				(insert! table keys value)
	    			)
	    	)

	    (define (dispatch m)
	      (cond ((eq? m 'lookup-proc) 
		     (lambda keys (lookup local-table keys)))
		    ((eq? m 'insert-proc!) 
		     (lambda (value . keys) 
		       (insert! local-table keys value) 
		       (display local-table)))
		    (else (error "Unknown operation - TABLE" m))
	      )
	    )
	    dispatch)
)




