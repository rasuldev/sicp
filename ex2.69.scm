(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) ; symbol
			       (cadr pair)) ; frequency
		    (make-leaf-set (cdr pairs))))))



(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;(make-leaf-set '((a 3) (b 2)))

(define (successive-merge nodes)
;  (display nodes)
;  (newline)
  (if (null? (cdr nodes))
      (car nodes)
      (successive-merge (adjoin-set (make-code-tree (car nodes) 
						    (cadr nodes))
				    (cddr nodes)
			)
      )
  )
)


;(define s-tree (generate-huffman-tree '((a 4) (b 2) (d 1) (c 1))))


;(make-code-tree (make-leaf 'a 4)
;		(make-leaf 'b 5))

;(right-branch s-tree)


;ex 2.70
(define s-tree (generate-huffman-tree 
		'((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1))))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
	  '()
	  (error "No code for symbol" symbol)
      )
      (let ((left (left-branch tree))
	    (right (right-branch tree)))
	(cond ((member symbol (symbols left))
	       (cons 0 (encode-symbol symbol left)))
	      ((member symbol (symbols right))
	       (cons 1 (encode-symbol symbol right)))
	      (else (error "No code for symbol" symbol))
	)
      )
  )      
)

(define mess '(Get a job Sha na na na na na na na na Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom))


(* (length mess) 3)

(length (encode mess s-tree))





