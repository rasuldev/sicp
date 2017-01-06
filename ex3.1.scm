(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum
  )
)

(define A (make-accumulator 10))
