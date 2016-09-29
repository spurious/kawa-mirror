(module-name (kawa swing))

(require <kawa.lib.prim_syntax>)

(import (kawa pictures) (kawa process-keywords))

(define (picture-jpanel pic)
 (gnu.kawa.swingviews.SwingPicture (->picture pic)))

(define-private (frame-keyword (frame :: <gnu.kawa.swingviews.SwingFrame>)
			       (name :: <java.lang.String>)
			       value)
  (cond ((string=? name "title")
	 (invoke frame 'setTitle value))
	((string=? name "menubar")
	 (invoke frame 'setJMenuBar value))
        ((string=? name "size")
         (frame:setSize value))
	(else (error (format "unknown frame attribute ~s" name)))))

(define-private (frame-non-keyword (frame :: <gnu.kawa.swingviews.SwingFrame>)
				    arg)
  :: <void>
  (invoke frame 'addComponent arg))

(define (frame #!rest args  :: <object[]>)
  :: <gnu.kawa.swingviews.SwingFrame>
  (let ((frame (make  <gnu.kawa.swingviews.SwingFrame> #!null #!null #!void)))
    (process-keywords frame args frame-keyword frame-non-keyword)
    (invoke frame 'pack)
    (invoke frame 'setVisible #t)
    frame))

(define current-frame (make-parameter #!null))
(define current-picture-panel (make-parameter #!null))
(define-private default-frame-size ::java.lang.ThreadLocal[java.awt.Dimension]
  (java.lang.ThreadLocal))

(define (set-frame-size! size::java.awt.Dimension #!optional (frame::javax.swing.JFrame #!null))
  (if frame
      (frame:setSize size)
      (let ((cur-frame ::javax.swing.JFrame (current-frame)))
        (default-frame-size:set size)
        (if cur-frame
            (cur-frame:setSize size)))))

(define (show-picture picture) ::void
  (define panel ::gnu.kawa.swingviews.SwingPicture (current-picture-panel))
  (cond ((eq? panel #!null)
         (set! panel (picture-jpanel picture))
         (current-picture-panel panel))
        (else
         (panel:setPicture picture)))
  (define fr ::javax.swing.JFrame (current-frame))
  (cond ((eq? fr #!null)
         (set! fr (frame title: "Kawa-picture" panel))
         (! sz (default-frame-size:get))
         (fr:setSize (if sz sz &D[400 400]))
         (current-frame fr))))

        
  
