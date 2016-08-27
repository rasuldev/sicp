(define (binom n k)
  (if (or (= k 0) (= k n))
      1
      (+ (binom (- n 1) (- k 1)) (binom (- n 1) k))
  )
)

(binom 5 3)
