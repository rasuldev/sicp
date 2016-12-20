(define (install-rational-package)
  ;...
  (define (equ-rat? rat1 rat2)
    (and (= (numer rat1) (numer rat2))
	 (= (denom rat1) (denom rat2))
    )
  )

  (put 'equ? '(rational rational) equ-rat?)
)


(define (install-rectangular-package)
  ;...
  (define (equ? z1 z2) 
    (and (= (real-part z1) (real-part z2))
	 (= (image-part z1) (image-part z2)))
  )
  (put 'equ? '(rectangular rectangular) equ?)
)

(define (install-polar-package)
  ;...
  (define (equ? z1 z2) 
    (and (= (magnitude z1) (magnitude z2))
	 (= (angle z1) (angle z2)))
  )
  (put 'equ? '(polar polar) equ?)
)



(define (install-complex-package)
  (define (make-rect-from-polar z-polar)
    (define make-from-mag-ang (get 'make-from-mag-ang 'rectangular))
    (make-from-mag-ang (magnitude z-polar)
		       (angle z-polar))
  )

  (define (equ-complex? z-polar z-rect)
    (equ? (make-rect-from-polar z-polar) z-rect)
  )

  (put 'equ? '(complex complex) equ?)
  (put 'equ? '(polar rectangular) equ-complex?)
  (put 'equ? '(rectangular polar) (lambda (z-rect z-polar) (equ-complex? z-polar z-rect)))
)

(define (equ-num-rat? num rat)
  (equ? (make-rational num 1) rat)
)

(put 'equ? '(scheme-number rational) 
(define (equ? num1 num2) (apply-generic 'equ? num1 num2))







