(define (make-action-listener proc)
  :: <java.awt.event.ActionListener>
  (invoke-static <gnu.kawa.swingviews.SwingContainer>
		 'makeActionListener proc))

(define (button #!key
		(label :: <String> #!null)
		(image #!null)
		(default #!null)
		(oncommand #!null)
		(disabled #f)
		(accesskey #!null))
  (let ((button :: <gnu.kawa.models.Button>
		(make  <gnu.kawa.models.Button>)))
    (if disabled
	(invoke button 'setDisabled #t))
    (if (not (eq? label #!null))
	(invoke button 'setLabel label))
    (if (not (eq? oncommand #!null))
	(invoke button 'setAction oncommand))
    button))

(define (fill (shape :: <java.awt.Shape>)) ::  <gnu.kawa.models.Paintable>
  (make <gnu.kawa.models.FillShape> shape))

(define (draw (shape :: <java.awt.Shape>)) ::  <gnu.kawa.models.Paintable>
  (make <gnu.kawa.models.DrawShape> shape))

(define (with-paint  (paint  :: <java.awt.Color>)
		     (pic ::  <gnu.kawa.models.Paintable>))
  (make  <gnu.kawa.models.WithPaint> pic paint))

(define (with-composite  #!rest (arguments :: <Object[]>))
  (gnu.kawa.models.WithComposite:make arguments))

(define (composite-src-over #!optional (alpha :: <float> 1.0))
  :: <java.awt.Composite>
  (java.awt.AlphaComposite:getInstance
   (static-field <java.awt.AlphaComposite> 'SRC_OVER)
   alpha))

(define (composite-src #!optional (alpha :: <float> 1.0))
  :: <java.awt.Composite>
  (java.awt.AlphaComposite:getInstance
   (static-field <java.awt.AlphaComposite> 'SRC)
   alpha))

(define (read-image url) :: <java.awt.image.BufferedImage>
  (javax.imageio.ImageIO:read (gnu.kawa.xml.Document:makeURL url)))

(define (rotation (theta :: <double>)) :: <java.awt.geom.AffineTransform>
  (java.awt.geom.AffineTransform:getRotateInstance theta))

(define (with-transform  (transform  :: <java.awt.geom.AffineTransform>)
		     (pic ::  <gnu.kawa.models.Paintable>))
  (gnu.kawa.models.WithTransform:new pic transform))

(define-constant color-red :: <java.awt.Color>
  (static-field <java.awt.Color> 'RED))

(define (frame #!rest args  :: <object[]>)
  :: <gnu.kawa.swingviews.SwingFrame>
    (let ((frame :: <gnu.kawa.swingviews.SwingFrame>
		 (make  <gnu.kawa.swingviews.SwingFrame> #!null #!null #!void))
	  (num-args :: <int> (field args 'length)))
      (let loop ((i :: <int> 0))
	(if (< i num-args)
	    (let ((arg ((primitive-array-get <object>) args i)))
	      (cond ((eq? arg title:)
		     (invoke frame 'setTitle
			     ((primitive-array-get <object>) args (+ i 1)))
		     (loop (+ i 2)))
		    ((eq? arg menubar:)
		     (invoke frame 'setJMenuBar
			     ((primitive-array-get <object>) args (+ i 1)))
		     (loop (+ i 2)))
		    (else
		     (invoke frame 'addComponent arg)
		     (loop (+ i 1))))))
	(invoke frame 'pack)
	(invoke frame 'show)
	frame)))



(define (menubar #!rest args  :: <object[]>)
    :: <javax.swing.JMenuBar>
    (let ((menubar :: <javax.swing.JMenuBar>
		    (make  <javax.swing.JMenuBar>))
	  (num-args :: <int> (field args 'length)))
      (let loop ((i :: <int> 0))
	(if (< i num-args)
	    (let ((arg ((primitive-array-get <object>) args i)))
	      
	      (invoke menubar 'add (as <javax.swing.JMenu> arg))
	      (loop (+ i 1)))))
      menubar))

(define (menu #!rest args :: <object[]>)
    :: <javax.swing.JMenu>
    (let ((menu :: <javax.swing.JMenu>
		    (make  <javax.swing.JMenu>))
	  (num-args :: <int> (field args 'length)))
      (let loop ((i :: <int> 0))
	(if (< i num-args)
	    (let ((arg ((primitive-array-get <object>) args i)))
	      (cond ((and (eq? arg label:) (< (+ i 1) num-args))
		     (invoke menu 'setText
			     (as <String>
				 ((primitive-array-get <object>) args (+ i 1))))
		     (loop (+ i 2)))
		    (else
		     (invoke menu 'add (as <javax.swing.JMenuItem> arg))
		     (loop (+ i 1)))))))
      menu))

(define (menuitem #!key
		(label :: <String> #!null)
		(image #!null)
		(default #!null)
		(oncommand #!null)
		(disabled #f)
		(accesskey #!null))
  :: <javax.swing.JMenuItem>
  (let ((menuitem :: <javax.swing.JMenuItem>
		(make  <javax.swing.JMenuItem>)))
    (if disabled
	(invoke menuitem 'setEnabled #f))
    (if (not (eq? label #!null))
	(invoke menuitem 'setText label))
    (if (not (eq? oncommand #!null))
	(invoke menuitem 'addActionListener (make-action-listener oncommand)))
    menuitem))

(define (polygon initial #!rest (more-points :: <object[]>))
  (let ((path :: <java.awt.geom.GeneralPath>
	      (make <java.awt.geom.GeneralPath>))
	(n-points :: <int>
		  ((primitive-array-length <object>) more-points)))
    (invoke path 'moveTo
	    (real-part initial) (imag-part initial))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i n-points)
	 (invoke path 'closePath)
	 path)
      (let ((pt ((primitive-array-get <object>) more-points i)))
	(invoke path 'lineTo (real-part pt) (imag-part pt))))))

(define (scroll contents #!key w h)
  (if (instance? contents <gnu.kawa.models.Paintable>)
      (set! contents (gnu.kawa.swingviews.SwingPaintable:new contents)))
  (let ((scr :: <javax.swing.JScrollPane>
	     (javax.swing.JScrollPane:new contents)))
    (invoke scr 'setPreferredSize (make <java.awt.Dimension> w h))
    scr))
