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

(define s (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))

(accumulate-n + 0 s)
