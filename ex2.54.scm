(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) true)
	((or (null? list1) (null? list2)) false)
	((and (pair? (car list1)) (pair? (car list2))) (and (equal? (car list1) (car list2))
							    (equal? (cdr list1) (cdr list2))))
	((or (pair? (car list1)) (pair? (car list2))) false)
	(else (and (eq? (car list1) (car list2))
		   (equal? (cdr list1) (cdr list2)))))
	  
)
  




(equal? '(a b c) '(a b c d))