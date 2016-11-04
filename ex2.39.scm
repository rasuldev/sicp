(define (reverse sequence)
  (fold-right (lambda (x acc) (append acc (list x))) 
	      '() 
	      sequence)
)


(reverse '(1 2 3))


(define (reverse-left sequence)
  (fold-left (lambda (acc x) (cons x acc)) '() sequence))

(reverse-left '(1 2 3))