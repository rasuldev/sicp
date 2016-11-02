(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile)
)

(define (right-branch mobile)
  (cadr mobile)
)

(define (branch-length branch)
  (car branch)
)

(define (branch-structure branch)
  (cadr branch)
)

(define (branch-weight branch)
    (let ((s (branch-structure branch)))
      (if (not (pair? s))
	  s
	  (mobile-weight s)
      )
    )
)

(define (mobile-weight mobile)
    (+ (branch-weight (left-branch mobile)) 
       (branch-weight (right-branch mobile)))
)

(define total-weight mobile-weight)

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch))
  )
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (= (torque left) (torque right))
	 (if (pair? (branch-structure left))
	     (balanced? (branch-structure left))
	     true
	 )
	 (if (pair? (branch-structure right))
	     (balanced? (branch-structure right))
	     true
	 )
     )	
  )
)


(define mobile (make-mobile (make-branch 3 2)
			    (make-branch 3 2)))
(define mobile2 (make-mobile (make-branch 3 mobile)
			     (make-branch 3 mobile)))




(total-weight mobile2)
(balanced? mobile2)


