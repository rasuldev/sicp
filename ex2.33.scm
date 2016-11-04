(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence))
      )
  )
)

(accumulate + 0 '(1 2 3))

(define (map p sequence)
  (accumulate (lambda (h acc) (cons (p h) acc))
	      ()
	      sequence
  )
)


(map square '(1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1)
)

(append '(1 2 3) '(4 5 6))


(define (length sequence)
  (accumulate (lambda (h acc) (+ acc 1)) 0 sequence)
)

(length '(1 2 3))

