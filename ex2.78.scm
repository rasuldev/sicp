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
  (newline)


  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (display proc)
      (if proc
	  (apply proc (map contents args))
	  (error "No method for these types - APPLY-GENERIC" (list op type-tags))
      )
    )
  )  
)

;======== NUMBER package =======

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

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
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done
)

(install-scheme-number-package)

(make-scheme-number 5)
;     (make-scheme-number 6))


(add 5 6)

;(get 'make 'scheme-number)

;  (put 'add '(scheme-number scheme-number)
;       (lambda (x y) (tag (+ x y))))



