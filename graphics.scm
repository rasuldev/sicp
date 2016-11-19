(define (cross)
  (graphics-draw-line device -1 -1  1 1)
  (graphics-draw-line device 1 -1  -1 1)

  (graphics-draw-line device -1 -1  -1 1)
  (graphics-draw-line device -1 1  1 1)
  (graphics-draw-line device 1 1  1 -1)
  (graphics-draw-line device 1 -1  -1 -1)

  (graphics-draw-line device -0.05 0.45 0 0.5)
  (graphics-draw-line device 0 0.5 0.05 0.45)

  (graphics-draw-line device 0.45 -0.05 0.5 0)
  (graphics-draw-line device 0.5 0 0.45 0.05)
)

(graphics-clear device)
(graphics-set-coordinate-limits device -1 -1 1 1)


(define (draw painter)
  (painter)
)

(define (display-coords)
  ((get-coords) (lambda (x0 y0 x1 y1)
		  (display (list x0 y0 x1 y1))
		)
  )
)

(define (get-coords)
  (graphics-coordinate-limits device)
)

(define (restore-coords coords)
  (coords (lambda (x0 y0 x1 y1)
	    (graphics-set-coordinate-limits device x0 y0 x1 y1)
	  )
  )
)

(define (painter-operation action)
  (lambda ()
    (let ((coords (get-coords)))
      (coords action)
      (restore-coords coords)
    )
  )
)

(define (beside left right)
  (painter-operation (lambda (x0 y0 x1 y1)    
		       ;(display-coords)
		       (graphics-set-coordinate-limits device (+ (* 2 x0) 1) y0 (+ (* 2 x1) 1) y1)
		       ;(display-coords)
		       (left)
		       ;(display-coords)
		       (graphics-set-coordinate-limits device (- (* 2 x0) 1) y0 (- (* 2 x1) 1) y1)
		       ;(display-coords)
		       (right)         
		     )
  )
)

(define (below bottom top)
  (painter-operation (lambda (x0 y0 x1 y1)
		       (display (list x0 y0 x1 y1))
		       (graphics-set-coordinate-limits device x0 (- (* 2 y0) 1) x1 (- (* 2 y1) 1))
		       (top)
		       (graphics-set-coordinate-limits device x0 (+ (* 2 y0) 1) x1 (+ (* 2 y1) 1))
		       (bottom)
		     )
  )
)

(define (flip-vert painter)
  (painter-operation (lambda (x0 y0 x1 y1)
		       (display (list x0 y0 x1 y1))
		       (graphics-set-coordinate-limits device x0 (* -1 y0) x1 (* -1 y1))
		       (painter)
		     )
  )
)

(define (flip-horiz painter)
  (painter-operation (lambda (x0 y0 x1 y1)
		       (graphics-set-coordinate-limits device (* -1 x0) y0 (* -1 x1) y1)
		       (painter)
		     )
  )
)



(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;(draw (below cross cross))
;(draw (beside cross (below cross (flip-vert cross))))
;(draw (flipped-pairs cross))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))


;(draw (right-split cross 10))
;(draw (beside cross (beside cross (beside cross cross))))
;(draw (below cross (beside cross (below cross (flip-vert cross)))))
;(draw (flipped-pairs cross))


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter 
	       (beside smaller smaller)
	)
      )
  )
)


;(draw (up-split cross 10))

(define (split bigOp smallOp)
  (define (splitter painter n)
    (if (= n 0)
	painter
	(let (
	      (smaller (splitter painter (- n 1))))
	  (bigOp painter
		 (smallOp smaller smaller)
	  )    
	)
    )
  )
  splitter
)

(draw ((split beside below) cross 4))








