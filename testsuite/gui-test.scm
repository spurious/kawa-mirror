(require 'gui)

(define test-image
  (read-image "file:///Users/bothner/pics/NelsonCove/NC-USGS-topo.jpg"))

(define Yes-button
  (button label: "Yes"
	  oncommand: (lambda ()
		       (format #t "Yes button pressed!~%~!"))))

(define p3 (polygon 0+0i 0+50i 50+50i))
(define oval (java.awt.geom.Ellipse2D$Float:new 10.0 10.0 50.0 30.0))
(define fill-p3 (fill p3))
(define draw-p3 (draw p3))

(frame title: "Kawa GUI Test"
       menubar: (menubar
		 (menu label: "File"
		       (menuitem label: "New"
				  oncommand: (lambda ()
					       (format #t "New menuitem pressed!~%~!")))
		       (menuitem label: "Close"
				 oncommand:
				 (lambda ()
				   (format #t "Close menuitem pressed!~%~!")
				   (exit))))
		 (menu label: "Edit"
		       (menuitem label: "Cut"
				 oncommand: (lambda ()
					       (format #t "Cut menuitem pressed!~%~!")))))
       Yes-button
       draw-p3
       (scroll
	(gnu.kawa.models.DrawImage:new test-image)
	h: 200 w: 200)
       Yes-button
       (with-composite
	(with-paint color-red fill-p3)
	(composite-src-over 0.5) 
	(fill oval))
       (with-composite draw-p3
		       (with-transform (rotation -0.5)
				       fill-p3)))
