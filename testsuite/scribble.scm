;; This is the scribble applet in Flanagan's "Java Examples in a Nutshell"
;; (O'Reilly, 1997), transcribed into Kawa-Scheme.

(define-private last-x 0)
(define-private last-y 0)

(define (init) :: <void>
  (let ((applet (this)))
    (invoke (this) 'addMouseListener
	    (object (<java.awt.event.MouseAdapter>)
		    ((mousePressed (e :: <java.awt.event.MouseEvent>)) <void>
		     (set! last-x (invoke e 'getX))
		     (set! last-y (invoke e 'getY)))))
    (invoke applet 'addMouseMotionListener
	    (object (<java.awt.event.MouseMotionAdapter>)
		    ((mouseDragged (e :: <java.awt.event.MouseEvent>)) <void>
		     (let ((g :: <java.awt.Graphics>
			      (invoke applet 'getGraphics))
			   (x :: <int> (invoke e 'getX))
			   (y :: <int> (invoke e 'getY)))
		       (invoke g 'drawLine last-x last-y x y)
		       (set! last-x x)
		       (set! last-y y)))))))

(define (start) <void> (format #t "called start.~%~!"))
(define (stop) <void> (format #t "called stop.~%~!"))
(define (destroy) <void> (format #t "called destroy.~%~!"))
