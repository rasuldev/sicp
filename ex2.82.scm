(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (all-of-one-type type-args)
	      (error "No method" (list op type-args))
	      (let ((coerced (coerce-to-one-type type-args)))
		(if coerced
		    (apply apply-generic (append (list op type1) coerced))
		    (error "No method" (list op type-args))
	        )
	      )  
	  )
       )
     )
  )
)


(define (coerce-to-one-type types)
  (define (carousel tested-types current-type non-tested-types)
    (let ((coerced1 (coerce current-type tested-types))
	  (coerced2 (coerce current-type non-tested-types)))
      (if (and coerced1 coerced2)
	  (append coerced1 (list current-type) coerced2)
	  (if (null? non-tested-types)
	      false
	      (carousel (append tested-types current-type)
			(car non-tested-types)
			(cdr non-tested-types))
	  )
      )
    )
  )
  (carousel '() (car types) (cdr types))
)

(define (coerce target-type types)
  (if (null? types)
      '()
      (let ((type (car types)))
	(let ((type->target-type (get-coercion type target-type)))
	  (if type->target-type
	      (cons (type->target-type type) (coerce target-type (cdr types)))
	      false
        )
      )
)


