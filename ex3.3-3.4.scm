(define (make-account balance password)
  (define fail-count 0)
  (define (call-the-cops)
    newline
    (display "Wiu-wiu...")
  )
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass password)
	(begin
	  (set! fail-count 0)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request - MAKE-ACCOUNT"
			     m)))
	)
	(begin
	  (set! fail-count (+ fail-count 1))
	  (if (= fail-count 7)
	      (call-the-cops))
	  ;(display fail-count)
	  (lambda (a) "Incorrect password")
        )
    )
  )
    
  dispatch
)

(define acc1 (make-account 100 'secret))

((acc1 'secret 'withdraw) 10)





