(define dispatch-table '())

(define (put op type proc)
  (cons (list op type proc) dispatch-table)
)

(define (get op type)
  (define (match? table-entry op type)
    (and (eq? (car table-entry) op)
	 (eq? (cadr table-entry) type))
  )
  (define (find table)
    (cond ((null? table) false)
	  ((match? (car table)) (caddr (car table)))
	  (else (find (cdr table)))
    )
  )
)

(define (install-deriv-package)
  (put 'deriv '+ (lambda (operands var) 
		   (make-sum (deriv (car operands) var)
			     (deriv (cadr operands) var))
		 )
  )

  (put 'deriv '* (lambda (operands var) 
		   (make-sum
		    (make-product (car operands)
				  (deriv (cadr operands) var))
		    (make-product (deriv (car operands) var)
				  (cadr operands)))
		 )
  )
)



(define (install-deriv-exp-package)
  (put 'deriv '** (lambda (operands var)
		    (let (((car operands) base)
			  ((cadr operands) exp))
		      (make-product exp
				    (** base (make-sum -1 exp))
				    (deriv base var))
		    ) 
		  )
  )
)
