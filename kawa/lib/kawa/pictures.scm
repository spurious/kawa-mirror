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

(define (picture-bounds picture)
  ((gnu.kawa.models.PBox:asPaintable picture):getBounds2D))

(define (hbox #!rest args  :: <object[]>)
  (gnu.kawa.models.PBox:makeHBox @args))
(define (vbox #!rest args  :: <object[]>)
  (gnu.kawa.models.PBox:makeVBox @args))
(define (zbox  #!rest args  :: <object[]>)
  (gnu.kawa.models.PBox:makeZBox @args))

(define (rectangle p1::java.awt.geom.Point2D #!optional (p2 #!null))
  ::java.awt.geom.Rectangle2D
  (let ((x p1:x)
        (y p1:y))
    (cond ((eq? p2 #!null)
           (java.awt.geom.Rectangle2D$Double 0 0 x y))
          ((java.awt.geom.Dimension2D? p2)
           (let ((p ::java.awt.geom.Dimension2D p2))
             (java.awt.geom.Rectangle2D$Double x y
                                               (+ x p:width) (+ y p:height))))
          (else
           (let ((p ::java.awt.geom.Point2D p2))
             (java.awt.geom.Rectangle2D$Double x y p:x p:y))))))

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

(define (circle radius::float
                #!optional (center::java.awt.geom.Point2D &P[0 0]))
  (let ((diam (* 2.0 radius)))
    (java.awt.geom.Ellipse2D:Double (- center:x radius) (- center:y radius)
                                    diam diam)))

;; TODO: (define (ellipse dimension #!optional center)

(define-procedure fill
  (lambda (shape ::java.awt.Shape) ::  <gnu.kawa.models.Paintable>
    (gnu.kawa.models.FillShape shape))
  (lambda (paint (shape :: <java.awt.Shape>))
    (with-paint paint (gnu.kawa.models.FillShape shape))))

(define (draw (shape :: <java.awt.Shape>)) ::  <gnu.kawa.models.Paintable>
  (make <gnu.kawa.models.DrawShape> shape))
;; TODO: (draw stroke-or-paint-specifier ... shape)

(define-procedure image
  (lambda (#!key src)
    (gnu.kawa.models.DrawImage src: src))
  (lambda (image ::java.awt.image.BufferedImage)
    (gnu.kawa.models.DrawImage image)))

(define (image-read (uri :: path)) :: <java.awt.image.BufferedImage>
  (javax.imageio.ImageIO:read (uri:openInputStream)))

(define (image-width (image  ::java.awt.image.RenderedImage)) ::int
  (*:getWidth image))

(define (image-height (image  ::java.awt.image.RenderedImage)) ::int
  (*:getHeight image))

(define (->paint value) :: <java.awt.Paint>
  (cond ((instance? value <java.awt.Paint>)
	 value)
	((instance? value <java.lang.Integer>)
	 (make <java.awt.Color> (java.lang.Integer:intValue value)))
	((instance? value <gnu.math.IntNum>)
	 (make <java.awt.Color> (gnu.math.IntNum:intValue value)))
	(else
         (let ((c (gnu.kawa.models.StandardColor:valueOf (value:toString))))
           (if (eq? c #!null)
               (primitive-throw (java.lang.ClassCastException (format "value ~a cannot be converted to a paint" value))))
           c))))

(define (->picture value)
  (gnu.kawa.models.PBox:asPaintable value))

(define (->transform tr)
  (->java.awt.geom.AffineTransform tr))

(define (with-paint paint pic)
  (gnu.kawa.models.WithPaint (->picture pic) (->paint paint)))

(define (with-transform tr pic)
  (let ((tr (->transform tr)))
    (cond ((java.awt.Shape? pic)
           (tr:createTransformedShape pic))
          ((java.awt.geom.AffineTransform? pic)
           (let ((trcopy (java.awt.geom.AffineTransform
                          (->java.awt.geom.AffineTransform tr))))
             (trcopy:concatenate pic)
             trcopy))
          ((java.awt.geom.Point2D? pic)
           (let ((dst (java.awt.geom.Point2D$Double)))
             (tr:transform (->java.awt.geom.Point2D pic) dst)))
          (else
           (gnu.kawa.models.WithTransform:new (->picture pic) tr)))))

(define (with-composite  #!rest (arguments :: <Object[]>))
  (gnu.kawa.models.WithComposite:make arguments))

(define-private (angle-to-radian val::java.lang.Number)::double
  (cond ((gnu.math.DQuantity? val)
         (let* ((qval ::gnu.math.DQuantity val)
                (unit (qval:unit))
                (dval (qval:doubleValue)))
           (cond ((eq? unit gnu.math.Unit:Empty)
                  (* dval (/ java.lang.Math:PI 180)))
                 ((or (eq? unit gnu.math.Unit:degree)
                      (eq? unit gnu.math.Unit:radian)
                      (eq? unit gnu.math.Unit:gradian))
                  dval)
                 ((error "invalid unit for angle")))))
        ((gnu.math.RealNum? val)
         (* val (/ java.lang.Math:PI 180)))
        ((gnu.math.Numeric? val)
         (error "not a real number for angle"))
        (else
         (* (val:doubleValue) (/ java.lang.Math:PI 180)))))

(define-procedure rotate
  (lambda (angle) (java.awt.geom.AffineTransform:getRotateInstance
                   (angle-to-radian angle)))
  (lambda (angle picture)
    (with-transform (rotate angle) picture)))

(define-procedure scale
  (lambda (sc::java.awt.geom.Point2D)
    (java.awt.geom.AffineTransform:getScaleInstance sc:x sc:y))
  (lambda (sc::real)
    (java.awt.geom.AffineTransform:getScaleInstance sc sc))
  (lambda (sc picture)
     (with-transform (scale sc) picture)))

(define-procedure translate
  (lambda (delta::java.awt.geom.Point2D)
    (java.awt.geom.AffineTransform:getTranslateInstance delta:x delta:y))
  (lambda (delta::java.awt.geom.Point2D picture)
     (with-transform (translate delta) picture)))
  
(define-procedure affine-transform
  (lambda (m00::double m10::double
           m01::double m11::double
           m02::double m12::double)
    (java.awt.geom.AffineTransform m00 m10 m01 m11 m02 m12))
  (lambda (px::java.awt.geom.Point2D
           py::java.awt.geom.Point2D
           p0::java.awt.geom.Point2D)
    (java.awt.geom.AffineTransform px:x px:y py:x py:y p0:x p0:y)))
