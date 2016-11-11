(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row
				    k
				    rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define nil '())
(define (flatmap proc seq)
  (fold-right append 
	      nil
	      (map proc seq)
  )
)

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b)))
)

(define empty-board nil)

; k queens positions in first k columns we will store in list of numbers (in reverse 
; order), where each number is row number of queen; all possible ways of placing
; k queens in k columns we will store in list of lists: ((1 3 5) (2 5 1) ...) - 
; list of positions

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens)
)

(define (zip-with-index seq)
  (define (iter head-index seq)
    (if (null? seq)
	nil
	(cons (cons head-index (car seq))
	      (iter (+ head-index 1) (cdr seq))
	)
    )
  )
  (iter 1 seq)
)

(define (contain? x seq)
  (cond ((null? seq) false)
	((= x (car seq)) true)
	(else (contain? x (cdr seq)))
  )
)


(define (on-diagonal? new-pos positions)
  (let ((indexed-positions (zip-with-index positions)))
    ; (row1,col1) and (row2,col2) will on the same diagonal
    ; if row1-col1=row2-col2 or row1+col1=row2+col2
    (or (contain? new-pos
		  (map (lambda (pos) (+ (car pos) (cdr pos)))
		       indexed-positions))
	(contain? new-pos
		  (map (lambda (pos) (- (cdr pos) (car pos)))
		       indexed-positions))
    )
    
  )
)

(define (safe? k positions)
  (if (null? positions)
      true
      (let ((head (car positions))
	    (rest (cdr positions)))
	(and (not (contain? head rest))
	     (not (on-diagonal? head rest))
	)
      )
  )
)




(length (queens 8))