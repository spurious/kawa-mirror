(module-name (kawa pictures))
(require <kawa.lib.prim_syntax>)
(define-private-alias WithPaint gnu.kawa.models.WithPaint)
(import (class gnu.kawa.models
               DDimension DrawImage Picture Pictures StandardColor)
        (class java.awt.geom AffineTransform)
        (class java.awt.image BufferedImage RenderedImage))

(define (make-Point x y) (java.awt.geom.Point2D$Double x y))
(define-simple-constructor P make-Point)
(define (make-Dimension w::double h::double) (DDimension w h))
(define-simple-constructor D make-Dimension)

(define (picture-bounds picture)
  ((gnu.kawa.models.Pictures:asPicture picture):getBounds2D))

(define (hbox #!rest args  :: <object[]>)
  (gnu.kawa.models.PBox:makeBox #\X @args))
(define (vbox #!rest args  :: <object[]>)
  (gnu.kawa.models.PBox:makeBox #\Y @args))
(define (zbox  #!rest args  :: <object[]>)
  (gnu.kawa.models.PBox:makeBox #\Z @args))

(define (rectangle p1::java.awt.geom.Point2D #!optional (p2 #!null))
  ::java.awt.geom.Rectangle2D
  (let ((x p1:x)
        (y p1:y))
    (cond ((eq? p2 #!null)
           (java.awt.geom.Rectangle2D$Double 0 0 x y))
          ((java.awt.geom.Dimension2D? p2)
           (let ((p ::java.awt.geom.Dimension2D p2))
             (java.awt.geom.Rectangle2D$Double x y p:width p:height)))
          (else
           (let* ((p ::java.awt.geom.Point2D p2)
                  (w (java.lang.Math:abs (- p:x x)))
                  (h (java.lang.Math:abs (- p:y y))))
             (java.awt.geom.Rectangle2D$Double x y w h))))))

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
  (lambda (shape ::java.awt.Shape) ::  <gnu.kawa.models.Picture>
    (gnu.kawa.models.FillShape shape))
  (lambda (paint (shape :: <java.awt.Shape>))
    (with-paint paint (gnu.kawa.models.FillShape shape))))

(define (draw #!rest args) ::gnu.kawa.models.Picture
  (gnu.kawa.models.DrawShape:makeDraw args))

(define (border-shape widths border::boolean picture::gnu.kawa.models.Picture)
  (let ((bounds (picture:getBounds2D)))
    (cond ((java.awt.geom.Dimension2D? widths)
           (let ((d ::java.awt.geom.Dimension2D widths))
             (gnu.kawa.models.Pictures:borderShape bounds border
                                                   d:height d:width
                                                   d:height d:width)))
          ((java.awt.geom.Rectangle2D? widths)
           (let ((d ::java.awt.geom.Rectangle2D widths))
             (gnu.kawa.models.Pictures:borderShape bounds border
                                                   (- bounds:y d:y) ; top
                                                   (- (+ d:x d:width) ; right
                                                      (+ bounds:x bounds:width))
                                                   (- bounds:x d:x) ; left
                                                   (- (+ d:y d:height) ; bottom
                                                      (+ bounds:y bounds:height)))))                                                  
          (else
           (let ((d ::double widths))
             (gnu.kawa.models.Pictures:borderShape bounds border d d d d))))))

(define-procedure border
  (lambda (widths paint picture)
    (let ((pic (->picture picture)))
      (gnu.kawa.models.PBox:makeBox #\Z
       (fill paint (border-shape widths #t pic))
       pic)))
  (lambda (widths picture)
    (let ((pic (->picture picture)))
      (gnu.kawa.models.PBox:makeBox #\Z
       (fill (border-shape widths #t pic))
       pic)))
  (lambda (picture)
    (border 1.0 picture)))

(define-procedure padding
  (lambda (widths background picture)
    (let ((pic (->picture picture)))
      (gnu.kawa.models.PBox:makeBox #\Z
       (fill background (border-shape widths #f pic))
       pic)))
  (lambda (widths picture)
    (padding widths StandardColor:transparent picture)))

(define-procedure image
  (lambda (#!key src)
    (gnu.kawa.models.DrawImage src: src))
  (lambda (image ::java.awt.image.BufferedImage)
    (gnu.kawa.models.DrawImage image))
  (lambda (image)
    (gnu.kawa.models.DrawImage (Pictures:toImage (->picture image)))))

(define (image-read (uri :: path)) :: <java.awt.image.BufferedImage>
  (javax.imageio.ImageIO:read (uri:openInputStream)))

(define (image-write picture path)::void
  (let* ((fname ::java.lang.String (path:toString))
         (lname (fname:toLowerCase))
         (format ::java.lang.String (cond ((lname:endsWith ".gif")
                        "gif")
                       ((or (lname:endsWith "jpg") (lname:endsWith ".jpeg"))
                        "jpg")
                       (else
                        "png")))
         (img (->image picture))
         (bimg (if (DrawImage? img) ((->DrawImage img):getImage) img))
         (strm ((gnu.kawa.io.Path:valueOf path):openOutputStream)))
    (try-finally
     (javax.imageio.ImageIO:write bimg format strm)
     (strm:close))))

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
         (let ((c (StandardColor:valueOf (value:toString))))
           (if (eq? c #!null)
               (primitive-throw (java.lang.ClassCastException (format "value ~a cannot be converted to a paint" value))))
           c))))

(define (->image value) ::RenderedImage
  (set! value (gnu.mapping.Promise:force value))
  (if (RenderedImage? value)
      value
      (Pictures:toImage (->picture value))))
      
(define (->picture value)
  (gnu.kawa.models.Pictures:asPicture value))

(define (->transform tr)
  (->AffineTransform tr))

(define (with-paint paint pic)
  (gnu.kawa.models.WithPaint (->picture pic) (->paint paint)))

(define (with-transform tr pic)
  (let ((tr (->transform tr)))
    (cond ((java.awt.Shape? pic)
           (tr:createTransformedShape pic))
          ((AffineTransform? pic)
           (let ((trcopy (AffineTransform
                          (->AffineTransform tr))))
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
  (lambda (angle) (AffineTransform:getRotateInstance
                   (angle-to-radian angle)))
  (lambda (angle picture)
    (with-transform (rotate angle) picture)))

(define-procedure scale
  (lambda (sc::java.awt.geom.Dimension2D)
    (AffineTransform:getScaleInstance sc:width sc:height))
  (lambda (sc::java.awt.geom.Point2D)
    (AffineTransform:getScaleInstance sc:x sc:y))
  (lambda (sc::real)
    (AffineTransform:getScaleInstance sc sc))
  (lambda (sc picture)
     (with-transform (scale sc) picture)))

(define-procedure translate
  (lambda (delta::java.awt.geom.Dimension2D)
    (AffineTransform:getTranslateInstance delta:width delta:height))
  (lambda (delta::java.awt.geom.Point2D)
    (AffineTransform:getTranslateInstance delta:x delta:y))
  (lambda (delta picture)
    (with-transform (translate delta) picture)))

(define-procedure re-center
  (lambda (xposition yposition picture)
    (let* ((pic (->picture picture))
           (bounds (pic:getBounds2D))
           (xgoal
            (cond ((eq? xposition 'left)
                   (bounds:getX))
                  ((eq? xposition 'right)
                   (+ (bounds:getX) (bounds:getWidth)))
                  ((or (eq? xposition 'center) (eq? xposition 'centre))
                   (+ (bounds:getX) (* 0.5 (bounds:getWidth))))
                  ((eq? xposition 'origin)
                   0)
                  (else (error "invalid x-position specifier" xposition))))
           (ygoal
            (cond ((eq? yposition 'top)
                   (bounds:getY))
                  ((eq? yposition 'bottom)
                   (+ (bounds:getY) (bounds:getHeight)))
                  ((or (eq? yposition 'center) (eq? yposition 'centre))
                   (+ (bounds:getX) (* 0.5 (bounds:getHeight))))
                  ((eq? yposition 'origin)
                   0)
                  (else (error "invalid y-position specifier" yposition)))))
      (with-transform (translate &P[(- xgoal) (- ygoal)]) pic)))
  (lambda (position picture)
    (cond ((or (eq? position 'left) (eq? position 'right))
           (re-center position 'center picture))
          ((or (eq? position 'top) (eq? position 'bottom))
           (re-center 'center position picture))
          (else
           (re-center position position picture))))
  (lambda (picture)
    (re-center 'center 'center picture)))
  
(define-procedure affine-transform
  (lambda (m00::double m10::double
           m01::double m11::double
           m02::double m12::double)
    (AffineTransform m00 m10 m01 m11 m02 m12))
  (lambda (px::java.awt.geom.Point2D
           py::java.awt.geom.Point2D
           p0::java.awt.geom.Point2D)
    (java.awt.geom.AffineTransform px:x px:y py:x py:y p0:x p0:y)))

(define (picture-write-svg picture (name ::path) #!optional (headers::boolean #t))::void
  (let ((port (gnu.kawa.io.OutPort:openFile name)))
    (try-finally
     (gnu.kawa.models.SVGUtils:toSVG (->picture picture) port headers)
     (port:close))))

(define (picture->svg-node picture)
  (gnu.kawa.models.SVGUtils:toSVGNode (->picture picture)))
