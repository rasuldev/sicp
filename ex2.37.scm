(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence))
      )
  )
)


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init 
			(map (lambda (seq) (car seq)) seqs))
	    (accumulate-n op init 
			  (map (lambda (seq) (cdr seq)) seqs))
      )
  )
)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))  
       m
  )
)

(define v '(1 2 3))
(define m (list '(1 0 1) '(0 1 0)))

(matrix-*-vector m v)



(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) 
	 m)))

(define m2 (list '(1 2) '(0 1) '(1 1)))
(matrix-*-matrix m m2)
