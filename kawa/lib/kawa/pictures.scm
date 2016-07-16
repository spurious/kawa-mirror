(module-name (kawa pictures))
(require <kawa.lib.prim_syntax>)

(define (make-Point x y) (java.awt.geom.Point2D$Double x y))
(define-simple-constructor P make-Point)
(define-simple-class Dimension (java.awt.geom.Dimension2D)
  (w ::double access: 'private)
  (h ::double access: 'private)
  ((getWidth) w)
  ((getHeight) h)
  ((setWidth wd::double)::void (set! w wd))
  ((setHeight ht::double)::void (set! h ht))
  ((setSize wd ht) (set! w wd) (set! h ht)))
(define (make-Dimension w::double h::double) (Dimension width: w height: h))
(define-simple-constructor D make-Dimension)

(define (hbox #!rest args  :: <object[]>)
  (gnu.kawa.models.PBox:makeHBox @args))
(define (vbox #!rest args  :: <object[]>)
  (gnu.kawa.models.PBox:makeVBox @args))

(define-private (line-path do-close::boolean (initial ::java.awt.geom.Point2D) #!rest (more-points :: <object[]>))
  (let ((path :: <java.awt.geom.GeneralPath>
	      (make <java.awt.geom.GeneralPath>))
        (curx::double initial:x)
        (cury::double initial:y)
	(n-points :: <int>
		  ((primitive-array-length <object>) more-points)))
    (path:moveTo curx cury)
    (do ((i :: <int> 0 (+ i 1)))
	((>= i n-points)
         (if do-close
             (invoke path 'closePath))
	 path)
      (let ((pt (more-points i)))
        (cond ((java.awt.geom.Dimension2D? pt)
               (let ((p ::java.awt.geom.Dimension2D pt))
                 (set! curx (+ curx p:width))
                 (set! cury (+ cury p:height))))
              (else
               (let ((p ::java.awt.geom.Point2D pt))
                 (set! curx p:x)
                 (set! cury p:y))))
	(path:lineTo curx cury)))))

(define (line initial #!rest (more-points :: object[]))
  (line-path #f initial @more-points))

(define (polygon initial #!rest (more-points :: object[]))
  (line-path #t initial @more-points))

(define (circle radius::float)
  (let ((negrad (- radius))
        (diam (* 2.0 radius)))
    (java.awt.geom.Ellipse2D:Float negrad negrad diam diam)))

(define (fill (shape :: <java.awt.Shape>)) ::  <gnu.kawa.models.Paintable>
  (make <gnu.kawa.models.FillShape> shape))

(define (draw (shape :: <java.awt.Shape>)) ::  <gnu.kawa.models.Paintable>
  (make <gnu.kawa.models.DrawShape> shape))

(define (image-read (uri :: path)) :: <java.awt.image.BufferedImage>
  (javax.imageio.ImageIO:read (uri:openInputStream)))

(define (image-width (image  :: <java.awt.image.BufferedImage>)) :: <int>
  (*:getWidth image))

(define (image-height (image  :: <java.awt.image.BufferedImage>)) :: <int>
  (*:getHeight image))
