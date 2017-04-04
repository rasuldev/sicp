; ex3.39
(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))

; 101: P1, P2
; 121: P2, P1
; 100: P1 (* x x), P2, P1 set x
; 11: P1 (* x x), P2 (+ x 1), P1 set x, P2 set x

; ex3.40
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

; 1) P1-access-x
; 2) P1-access-x
; 3) P1-set-x
; 4) P2-access-x
; 5) P2-access-x
; 6) P2-access-x
; 7) P2-set-x

; 1-7: 1 000 000
; 4-7-1-3: 1 000 000
; 1-2-4-3

; ex3.43
(define (exchange account1 account2)
  (let ((difference (- (account1 ’balance)
                       (account2 ’balance))))
    ((account1 ’withdraw) difference)
    ((account2 ’deposit) difference)))


; a1:10 a2:20 a3:30
; a1 <-> a2; a2 <-> a3 => a1:20 a2:30 a3:10
; 1) a1-a2:10 - 20 = -10 
; 2) a2 <-> a3 (a1:10 a2:30 a3:20)
; 3) a1:20 a2:20 a3:20

(define (make-semaphore n)
  (define (make-mutex-list n)
    (if (= n 0)
        '()
        (cons (make-mutex) (make-mutex-list (- n 1)))))
  
  (define (acquire mutex-list)
    (define (iter list)
      (cond ((null? list) (acquire mutex-list))
            (((car list) 'acquired?) (iter (cdr list)))
            (else ((car list) 'acquire)))))
  
  (define (release mutex-list)
    (cond ((null? mutex-list) 'ok) 
          (((car list) 'acquired?) ((car list) 'release))
          (else (release (cdr mutex-list)))))
  
  (let ((mutexes (make-mutex-list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire mutexes)) 
            ((eq? m 'release) (release mutexes))))
    the-semaphore))

(define (make-semaphore2 n)
  (define (make-list length value)
    (if (= length 0)
        '()
        (cons value (make-list (- length 1) value))))
  (define (test-and-set! cells)
    (cond ((null? cells) true)
          ((car cells) (test-and-set! (cdr cells)))
          (else (set-car! cells true)
                false)))
  (define (clear cells)
    (cond ((null? cells) 'ok)
          ((car cells) (set-car! cells false))
          (else (clear (cdr cells)))))
    
  (let ((cells (make-list n false)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (if (test-and-set! cells)
                                  (the-semaphore 'acquire)))
            ((eq? m 'release) (clear cells))))))