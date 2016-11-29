(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
((=number? m1 1) m2)
((=number? m2 1) m1)
((and (number? m1) (number? m2)) (* m1 m2))
(else (list '* m1 m2))))

(define (=number? exp num)
(and (number? exp) (= exp num)))

(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
((=number? a2 0) a1)
((and (number? a1) (number? a2)) (+ a1 a2))
(else (list '+ a1 a2))))

(define (make-sum . summands)
  (define (sum summands)
    (cond ((= (length summands) 2) (list '+ (car summands) (cadr summands)))
	  ((=number? (car summands) 0) (sum (cdr summands)))
	  ((and (number? (car summands)) (number? (cadr summands))) (sum (cons (+ (car summands) (cadr summands)) 
									       (cddr summands))))
	  (else (list '+ (car summands) (sum (cdr summands))		      
    )   
  ))
  )
  (sum summands)
)

(make-sum 0 2 3)


(define (multiplicand p) (caddr p))

(define (multiplier p) (cadr p))

(define (augend s) (caddr s))

(define (addend s) (cadr s))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (variable? x) (symbol? x))

(define (product? x)
(and (pair? x) (eq? (car x) '*)))

(define (augend s) (caddr s))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**))
)

(define (base e)
  (cadr e)
)

(define (exponent e)
  (caddr e)
)

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
	((=number? exp 1) base)
	(else (list '** base exp)))
)



(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (make-product (exponent exp)
				     (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
		       (deriv (base exp) var)))
	(else
	 (error "unknown expression type - DERIV" exp))))

(deriv '(** x 2) 'x)


;(define exp '(** x 2))





