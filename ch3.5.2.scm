; ex 3.50
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))		
      the-empty-stream
      (cons-stream
        (apply proc (map car argstreams))
        (apply stream-map
               (cons proc (map cdr argstreams))))))




; ex 3.51
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      ’done
      (begin (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))
;==========

(define (show x)
  (display-line x)
  x)

(define x (stream-map show
                      (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

; tracing
; output: 0 1 2 3 4 5
(cons-stream 0 (stream-map show (stream-enumerate-interval 1 10))) ; = x



(stream-ref x 5)
(stream-ref (cons-stream 1 
                         (stream-map show (stream-enumerate-interval 2 10))) 4)

(stream-ref (cons-stream 2
                         (stream-map show (stream-enumerate-interval 3 10))) 3)

(stream-ref (cons-stream 5
                         (stream-map show (stream-enumerate-interval 6 10))) 0)

; x = 
(cons-stream 0 (cons-stream 1 (stream-map show (stream-enumerate-interval 2 10))))

;=======ex 3.52==========
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

sum=0
seq=(cons-stream 1 (stream-map accum (stream-enumerate-interval 2 10))) 
y= (stream-filter pred (cons-stream 3 (stream-map accum (stream-enumerate-interval 3 10)))

seq=(cons-stream 1 (cons-stream 3 (stream-map accum (stream-enumerate-interval 3 10))))
y= (stream-filter pred (stream-map accum (stream-enumerate-interval 3 10)))
y= (stream-filter pred (cons-stream 6 (stream-map accum (stream-enumerate-interval 6 10)))
y= (cons-stream 6 (stream-filter pred (stream-map accum (stream-enumerate-interval 6 10))))
seq=(cons-stream 1 (cons-stream 3 (cons-stream 6 (stream-map accum (stream-enumerate-interval 4 10))))

z=(stream-filter mul-of-5 (cons-stream 10 (stream-map accum (stream-enumerate-interval 5 10)))
z=(cons-stream 10 (stream-filter mul-of-5 (stream-map accum (stream-enumerate-interval 5 10))))
seq=(cons-stream 1 (cons-stream 3 (cons-stream 6 (cons-stream 10 (stream-map accum (stream-enumerate-interval 5 10))))
;===============
(define double (cons-stream 1 (scale-stream double 2)))
(stream-cdr double)
(scale-stream double 2)
(stream-map mul-by-2 (stream-cdr double))

double=(cons-stream 1 (cons-stream 2 (stream-map mul-by-2 (stream-cdr double))))


                 