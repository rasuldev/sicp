;(define (cons-stream a b)
;  (cons a (delay b)))

(require 'macro-by-example)

;(define-syntax delay
;  (syntax-rules ()
;    ((delay a)
;     (lambda () a))))

;(define (force a) (a))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))
;(defmacro (cons-stream a b)
;          `(cons a (delay b)))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      ’done
      (begin (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-all-stream s)
  (stream-for-each display-line s))

(define (display-stream s n)
  (if (and (> n 0) (not (stream-null? s)))
      (begin
        (display (stream-car s))
        (display " ")
        (display-stream (stream-cdr s) (- n 1)))))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (display argstreams)
  (display (cons proc (map stream-cdr argstreams)))
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))
;================================================
;(define (show x)
;  (display-line x)
;  x)

;(define x (stream-map show
;                      (stream-enumerate-interval 0 10)))

;(define stream (cons-stream 1 ((lambda () 
;                                (display "1")
;                                2))))

;====================
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
;define integers (streams-add ones integers))

(define s1 (list 1 2 3))
(define s2 (list 1 2 3))
(define s3 (list 1 2 3))

(define sum (stream-map + ones ones))

;(display-stream integers 20)

(define (partial-sums s)
  (cons-stream (stream-car s)
               (streams-add (stream-cdr s) (partial-sums s))))


