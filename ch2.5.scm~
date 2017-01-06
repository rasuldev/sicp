;==== DISPATCH routines ======

(define dispatch-table '())

(define (put op type proc)
  (set! dispatch-table (cons (list op type proc) dispatch-table))
)

(define (get op types)
  (define (match? table-entry)
    (and (eq? (car table-entry) op)
	 (equal? (cadr table-entry) types))
  )
  (define (find-in table)
    (cond ((null? table) false)
	  ((match? (car table)) (caddr (car table)))
	  (else (find-in (cdr table)))
    )
  )
  (find-in dispatch-table)
)

;====== TAG routines =====

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)
  )
)

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else (error "Bad tagged datum - TYPE-TAG" datum))
  )
)

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else (error "Bad tagged datum - CONTENTS" datum))
  )
)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error "No method for these types - APPLY-GENERIC" (list op type-tags))
      )
    )
  )  
)
;======== GENERIC OPERATIONS ======
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (neg x) (apply-generic 'neg x))
(define (print x) (apply-generic 'print x))

;========= NUMBER PACKAGE ========
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(scheme-number) 
       (lambda (x) (* -1 x)))
  (put '=zero? '(scheme-number) 
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'print '(scheme-number) display)
  'done
)


;====== POLYNOMIAL PACKAGE ========
(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (same-variable? x y)
    (eq? x y)
  )
  (define (variable? x) (symbol? x))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (make-term-list coeffs)
    (cond ((null? coeffs)( the-empty-termlist))
	  (else (adjoin-term (make-term (length coeffs) (car coeffs))
			     (make-term-list (cdr coeffs)))
		
	  )
    )
	  
  )
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (make-poly (variable p1) 
	       (add-terms (term-list p1)
			  (term-list p2)))
  )

  (define (mul-poly p1 p2)
    (make-poly (variable p1) 
	       (mul-terms (term-list p1)
			  (term-list p2)))
  )

  (define (=zero-poly? p)
    (empty-termlist? (term-list p))
  )

  (put '=zero? '(polynomial) =zero-poly?)


  (define (negate-poly p)
    (define (negate-term term)
      (make-term (order term) (negate (coeff term)))
    )

    (define (negate-terms terms)
      (adjoin-term (negate-term (first-term terms))
		   (negate-terms (rest-terms terms)))
      
    )
    
    (make-poly (variable p) (negate-terms (term-list p)))
  )

  (define (sub p1 p2)
    (add p1 (negate p2))
  )


  (define (print p)
    (define (print-term term)
      (display (coeff term))
      (display (variable p))
      (display "^")
      (display (order term))
    )
    
    (define (print-terms terms)
      (if (empty-termlist? terms)
	  (if (=zero? p) 
	      (display 0)
	      (display ""))
	  (begin
	    (print-term (first-term terms))
	    (print-terms (rest-terms terms)))
      )
    )
    newline
    (print-terms (terms (term-list p)))
  )


  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'make 'term-list make-term-list)
  (put 'adjoin 'term adjoin-term)
  
  'done
)

(install-scheme-number-package)
(install-polynomial-package)

(define (make-terms coeffs)
  ((get 'make 'term-list) coeffs)
)

(define (make-polynom var terms)
  ((get 'make 'polynom) var terms)
)

(define (adjoin term list)
  ((get 'adjoin 'term) term list)
)

(define terms (make-terms (list 1 2 3)))

terms

(define p (make-polynom 'x terms))


 





