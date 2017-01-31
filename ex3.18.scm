(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define (has-cycle x)
  (define (new-set initial-elements) 
    (define (add x)
      (let ((is-x-new (not (pair? (memq x initial-elements)))))
	(if is-x-new
	    (set! initial-elements (cons x initial-elements)))
	is-x-new
       )
    )  
    add
  )

  (define add-to-set (new-set '()))

  (define (find-cycle x)
    (cond ((not (pair? x)) false)
	  ((add-to-set x) (or (find-cycle (car x)) 
			      (find-cycle (cdr x))))
	  (else true)
    )
  )
  (find-cycle x)
)

(has-cycle '(a b c))
(has-cycle (make-cycle '(a b c)))
(has-cycle (cons (make-cycle '(a b c)) 'd))
