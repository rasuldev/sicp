(define (count-pairs x)
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

  (define (count-iter x)
    (if (not (pair? x))
	0
	(if (add-to-set x)
	    (+ (count-iter (car x))
	       (count-iter (cdr x))
	       1
	    )
	    0
	)

    )
  )
  (count-iter x)
)



(count-pairs '(a b c))

(define x '(a b))

(define last (cdr x))

(set-cdr! last x)

(count-pairs x)

