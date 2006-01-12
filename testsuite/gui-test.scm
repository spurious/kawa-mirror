(require 'swing-gui)

(define test-image
  (image-read "http://nelsoncove.us/NC-USGS-topo.jpg"))

(define Yes-button :: <gnu.kawa.models.Button>
  (Button text: "Yes"
	  action: (lambda (e)
		    (*:setText Yes-button "Yes!")
		    (format #t "Yes button pressed!~%~!"))))

(define p3 (polygon 0+0i 0+80i 150+20i))
(define oval (java.awt.geom.Ellipse2D$Float:new 10.0 10.0 50.0 30.0))
(define fill-p3 (fill p3))
(define draw-p3 (draw p3))
(define text-field-1 (make <gnu.kawa.models.Text> "Hello ..."))

(run-application
 (Window title: "Kawa GUI Test"
#|
       menubar: (menubar
		 (menu label: "File"
		       (menuitem label: "New"
				  oncommand: (lambda (e)
					       (format #t "New menuitem pressed!~%~!")))
		       (menuitem label: "Close"
				 oncommand:
				 (lambda (e)
				   (format #t "Close menuitem pressed!~%~!")
				   (exit))))
		 (menu label: "Edit"
		       (menuitem label: "Cut"
				 oncommand: (lambda (e)
					       (format #t "Cut menuitem pressed!~%~!")))))
|#
       (Row
	draw-p3
       (Column
	text-field-1
	Yes-button
	text-field-1
	Yes-button)
       (scroll  (Image test-image) h: 200 w: 200)
       (with-composite
	(with-paint color-red fill-p3)
	(composite-src-over 0.5)
	(fill oval))
       (with-composite draw-p3
		       (with-transform (rotation -0.5)
				       fill-p3))))
)
