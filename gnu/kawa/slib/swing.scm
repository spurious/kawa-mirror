(module-export make-action-listener fill draw set-content
	       with-paint with-composite composite-src-over composite-src
	       button Button Label Column Row Text Window run-application
	       Image image-read image-width image-height
	       rotation with-transform color-red menubar menu menuitem
	       polygon scroll frame picture->jpanel)

(module-compile-options warn-undefined-variable: #t
			  warn-invoke-unknown-method: #t)

(require 'gui)
(import (except (kawa pictures) polygon)
        (kawa swing))

(define (make-action-listener proc)
  :: <java.awt.event.ActionListener>
  (invoke-static <gnu.kawa.swingviews.SwingDisplay>
		 'makeActionListener proc))

#|
(define (Window #!rest args  :: <object[]>)
  :: <gnu.kawa.swingviews.SwingFrame>
  (let ((frame (make  <gnu.kawa.swingviews.SwingFrame> #!null #!null #!void)))
    (process-keywords frame args frame-keyword frame-non-keyword)
    ;(invoke frame 'pack)
    ;(invoke frame 'setVisible #t)
    frame))
|#

#|
(define (frame #!rest args  :: <object[]>)
  :: <gnu.kawa.swingviews.SwingFrame>
    (let ((frame :: <gnu.kawa.swingviews.SwingFrame>
		 (make  <gnu.kawa.swingviews.SwingFrame> #!null #!null #!void))
	  (num-args :: <int> (field args 'length)))
      (let loop ((i :: <int> 0))
	(if (< i num-args)
	    (let ((arg ((primitive-array-get <object>) args i)))
	      (cond ((instance? arg <gnu.expr.Keyword>)
		     (frame-keyword frame (gnu.expr.Keyword:getName arg)
				    ((primitive-array-get <object>) args (+ i 1)))
		     (loop (+ i 2)))
		    ((instance? arg <gnu.kawa.xml.KAttr>)
		     (let* ((attr :: <gnu.kawa.xml.KAttr> arg)
			    (name :: <java.lang.String> (invoke attr 'getName))
			    (value (invoke attr 'getValue))) ;; FIXME
		       (frame-keyword frame name value))
		     (loop (+ i 1)))
		    (else
		     (invoke frame 'addComponent arg)
		     (loop (+ i 1))))))
	(invoke frame 'pack)
	(invoke frame 'show)
	frame)))
|#


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
	      (cond ((and (eq? arg 'label:) (< (+ i 1) num-args))
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

(define (scroll contents #!key w h)
  (if (instance? contents <gnu.kawa.models.Picture>)
      (set! contents (gnu.kawa.swingviews.SwingPicture:new contents)))
  (let ((scr :: <javax.swing.JScrollPane>
	     (javax.swing.JScrollPane:new contents)))
    (invoke scr 'setPreferredSize (make <java.awt.Dimension> w h))
    scr))
