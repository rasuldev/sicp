(define (make-table same-key?)

  (define (assoc key records)
    (cond ((null? records) false)
	  ((same-key? key (caaar records)) (car records))
	  (else (assoc key (cdr records))))
  )

  (let ((local-table (list '*table*)))
    (define (lookup table keys)
      (let ((subtable (assoc (car keys) (cdr table))))
	(if subtable
	    (if (null? (cdr keys))
		(cdar subtable)
		(lookup subtable (cdr keys))
	    )
	    false
	)
      )
    )

    (define (insert! table keys value)
;      (display (list table keys value))
;      (display "|")
      (let ((subtable (assoc (car keys) (cdr table))))
	(if subtable
	    (if (null? (cdr keys))
		(set-cdr! (car subtable) value)
		(insert! subtable (cdr keys) value)
	    )
	    (let ((new-subtable (cons (cons (car keys) false) 
				      '()
                                )))
	      (set-cdr! table 
			(cons new-subtable (cdr table)))
	      (insert! table keys value)
	    )
	 )
      )
      'ok
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
    dispatch
  )
)

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;(put 1 1)



