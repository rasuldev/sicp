;=========Tree========
(define (make-tree root-entry key-selector)

	(define (make-tree entry left right)
  		(list entry left right))
	(define (tree-entry tree) (car tree))
	(define (left-branch tree) (cadr tree))
	(define (right-branch tree) (caddr tree))
	(define (tree-key tree) (key-selector (tree-entry tree)))

	(let ((local-tree (make-tree root-entry '() '())))
		(define (adjoin entry tree)
		  ;(display "adjoin: ")
		  ;(write-line (list entry tree))
		  (cond ((null? tree) 
			 ;(write-line "tree is null")
			 (make-tree entry '() '()))
			((= (key-selector entry) (tree-key tree)) tree)
			((< (key-selector entry) (tree-key tree))
			 (make-tree (tree-entry tree)
				    (adjoin entry (left-branch tree))
				    (right-branch tree)))
			((> (key-selector entry) (tree-key tree))
			 (make-tree (tree-entry tree)
				    (left-branch tree)
				    (adjoin entry (right-branch tree))))))

		(define (lookup entry-key tree)
;		  (display "tree lookup:")
;		  (write-line (list entry-key tree))
			(cond ((null? tree) false)
				((= entry-key (tree-key tree)) (tree-entry tree))
				((< entry-key (tree-key tree)) (lookup entry-key (left-branch tree)))
				(else (lookup entry-key (right-branch tree)))))

		(define (dispatch m)
		  (cond ((eq? m 'adjoin) 
			 (lambda (entry) 
			   (set! local-tree (adjoin entry local-tree)) 
;			   (write-line "adjoin end")   
			    ))
			((eq? m 'lookup) 
			 (lambda (entry-key) (lookup entry-key local-tree)))
			((eq? m 'entry) (tree-entry local-tree))
			(else (error "MAKE-TREE no such command" m))))
		dispatch
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

	(define (make-table key value)
		(list key value '())
	)

	(define (get-table-key table)
		(car table))

	(define (get-table-value table)
		(cadr table))

	(define (set-table-value table value)
		(set-car! (cdr table) value))

	; field records points to tree root
	(define (get-table-records table)
		(caddr table))

	(define (set-table-records table records)
		(set-car! (cddr table) records))

   	(let ((local-table (make-table '*table* false)))
	    (define (lookup table keys)
	      (display "table lookup:")
	      (write-line (list table keys))
	    	(if (null? keys)
	    		(get-table-value table)
	    		(let ((records-tree (get-table-records table)))
			  (write-line records-tree)
			    (if (null? records-tree)
				false
				(let ((subtable ((records-tree 'lookup) (car keys))))
				  (if subtable
				      (lookup subtable (cdr keys))
				      false))))))

	    (define (insert! table keys value)
	      ;(write-line table)
	      
	      (if (null? keys)
		  (set-table-value table value)
		  (let ((records (get-table-records table)))
		    (if (not (null? records))
			;has records
			(let ((subtable ((records 'lookup) (car keys))))
			  (if subtable
			      ;found subtable in records
			      (insert! subtable (cdr keys) value)
			      ;no subtable in records: creating new
			      (let ((new-subtable (make-table (car keys) false)))
;				(write-line "before")
				((records 'adjoin) new-subtable)
;				(write-line "after")
				(insert! new-subtable (cdr keys) value))))
			;no records: creating new records (tree) with new table as root
			(let ((new-subtable (make-table (car keys) false)))
			  (set-table-records table
					     (make-tree new-subtable get-table-key))
			  (insert! new-subtable (cdr keys) value))))))
		    	

	    (define (dispatch m)
	      (cond ((eq? m 'lookup-proc) 
		     (lambda keys (lookup local-table keys)))
		    ((eq? m 'insert-proc!) 
		     (lambda (value . keys) 
		       (insert! local-table keys value) 
		       ;(display local-table)
		       ))
		    (else (error "Unknown operation - TABLE" m))
	      )
	    )
	    dispatch)
)


(define operation-table (make-bin-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(put 'a 1)
(put 'b 2)
;(get 1)


