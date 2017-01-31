(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    
    (define (empty?)
      (null? front-ptr)
    )

    (define (front)
      (if (empty?)
	  (error "FRONT called on an empty queue")
	  (car front-ptr)
      )
    )
    
    (define (insert! item)
      (let ((new-pair (cons item '())))
	(if (empty?)
	    (set! front-ptr new-pair)
	    (set-cdr! rear-ptr new-pair))
	(set! rear-ptr new-pair)	
      )
    )

    (define (delete!)
      (if (empty?)
	  (error "DELETE! called on an empty queue")
	  (set! front-ptr (cdr front-ptr))
      )
    )

    (define (print)
      (display front-ptr)
    )

    (define (dispatch m)
      (cond ((eq? m 'empty) (empty?))
	    ((eq? m 'front) (front))
	    ((eq? m 'insert) insert!)
	    ((eq? m 'delete) (delete!))
	    ((eq? m 'print) (print))
	    (else (error "DISPATCH no such procedure" m))
      )
    )
    
    dispatch
  )
)


(define q1 (make-queue))
((q1 'insert) 2)
((q1 'insert) 1)
((q1 'insert) 0)
(q1 'delete)
(q1 'empty)
(q1 'print)






