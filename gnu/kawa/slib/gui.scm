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

(define (frame #!key
	       (title  :: <String> #!null)
	       (menubar ::  <javax.swing.JMenuBar> #!null)
	       contents)
  :: <gnu.kawa.swingviews.SwingFrame>
  (make  <gnu.kawa.swingviews.SwingFrame> title menubar contents))

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

(define (panel #!key
	       (width :: <integer> 10)
	       (height :: <integer> 10)
	       contents)
  :: <javax.swing.JPanel>
  (let ((panel :: <javax.swing.JPanel>
	       (make <javax.swing.JPanel>)))
    (invoke panel 'setPreferredSize (make <java.awt.Dimension> width height))
    panel))
