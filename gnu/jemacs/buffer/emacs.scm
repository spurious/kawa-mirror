(define (set-size win w h)
  ((primitive-virtual-method <java.awt.Component> "setSize" <void>
			     (<int> <int>))
   win w h))

;;; From here on, the functions are ordered as in the Lisp Reference
;;; Manual, XEmacs version 21.x.

;;; READ AND PRINT

(define (open-output-buffer (buffer <buffer>))
  (make <gnu.jemacs.buffer.BufferWriter> buffer))

(define (open-output-marker (marker <gnu.jemacs.buffer.Marker>))
  (make <gnu.jemacs.buffer.BufferWriter> marker #f))

;;; MINIBUFFERS

(define (read-dialog prompt)
  (symbol->string
   (invoke (as <gnu.jemacs.buffer.Frame> (window-frame)) 'ask prompt)))

(define read-from-minibuffer read-dialog)

;;; KEYMAPS

(define (make-keymap #!optional name)
  (invoke-static <gnu.jemacs.buffer.BufferKeymap> 'makeEmptyKeymap
                 (as <String> name)))

(define (keymap-name (keymap <javax.swing.text.Keymap>))
  (invoke keymap 'getName))

(define global-map
  (invoke-static <javax.swing.text.JTextComponent> 'getKeymap
                 (static-field <javax.swing.text.JTextComponent>
                               'DEFAULT_KEYMAP)))

(define (current-global-map)
  global-map)

(define (current-local-map #!optional (buffer :: <buffer> (current-buffer)))
  (invoke (field buffer 'keymap) 'getLocalKeymap))

(define (use-local-map keymap #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.BufferKeymap> "setLocalKeymap"
                             <void> (<javax.swing.text.Keymap>))
   ((primitive-get-field <buffer> "keymap"
                         <gnu.jemacs.buffer.BufferKeymap>)
    buffer)
   keymap))

(define (lookup-key keymap keys #!optional accept-defaults)
  (let ((binding
         ((primitive-static-method <gnu.jemacs.buffer.BufferKeymap> "lookupKey"
                                   <object> (<javax.swing.text.Keymap>
                                             <gnu.kawa.util.Sequence> <boolean>))
          keymap keys accept-defaults)))
    (if (eq? binding #!null) #f binding)))

(define (define-key keymap key binding)
  ((primitive-static-method <gnu.jemacs.buffer.BufferKeymap> "defineKey" <void>
			    (<javax.swing.text.Keymap> <object> <object>))
   keymap key binding))

;;; FILES

(define (find-file #!optional (filename (read-from-minibuffer "Find file: ")))
  (switch-to-buffer (find-file-noselect filename)))

(define (find-file-noselect
         #!optional (filename (read-from-minibuffer "Find file: ")))
  ((primitive-static-method <buffer> "findFile"
                            <buffer> (<String>))
   filename))

(define (save-buffer #!optional (buffer (current-buffer)))
  (if (buffer-file-name buffer)
      ((primitive-virtual-method <buffer> "save" <void> ())
       (current-buffer))
      (write-file (read-from-minibuffer "File to save in: ") buffer)))

(define (write-file #!optional (filename (read-from-minibuffer "Write-file: "))
                    (buffer (current-buffer)))
  (set-visited-file-name filename buffer)
  (save-buffer buffer))

(define (insert-file filename #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "insertFile"
                             <void> (<String>))
   buffer filename))

;;; BUFFERS

(define (current-buffer)
  (invoke-static <buffer> 'getCurrent))

;; Emacs allows a buffer name as well as a buffer.
(define (set-buffer buffer)
  (invoke-static <buffer> 'setCurrent buffer))

;; Emacs returns an Emacs string, not a Java string. 
(define (buffer-name #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "getName"
			       <String> ())
   buffer))


(define (get-buffer buffer-or-name)
  ((primitive-static-method  <buffer> "coerceBuffer"
			     <buffer> (<object>))
   buffer-or-name))

(define (generate-new-buffer-name starting-name)
  ((primitive-static-method <buffer> "generateNewBufferName"
			    <String> (<String>))
   starting-name))

(define (buffer-file-name #!optional (buffer (current-buffer)))
  (let ((name
         ((primitive-virtual-method <buffer> "getFileName"
                                    <java.lang.String> ())
          buffer)))
    (if (eq? name #!null)
        #f
        (symbol->string name))))

(define (set-visited-file-name filename #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "setFileName" <void>
                             (<String>))
   buffer filename))

(define (get-buffer-create name)
  (let ((buf
	 ((primitive-static-method  <buffer> "getBuffer"
				    <buffer> (<String>))
	  name)))
    (if (eq? buf #!null)
	((primitive-constructor <buffer> (<String>)) name)
	buf)))

(define (generate-new-buffer name)
  ((primitive-constructor <buffer> (<String>))
   (generate-new-buffer-name name)))

;;; WINDOWS

(define (split-window #!optional (window (selected-window)) (size -1) (horizontal #f))
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "split"
			     <gnu.jemacs.buffer.Window> (<int> <boolean>))
   window size horizontal))

(define (split-window-vertically #!optional (arg -1))
  ; "Split current window into two windows, one above the other."
  ; (interactive "P")
  (split-window (selected-window) arg #f))

(define (split-window-horizontally #!optional (arg -1))
  ; "Split current window into two windows, one above the other."
  ; (interactive "P")
  (split-window (selected-window) arg #t))

(define (delete-window #!optional (window (selected-window)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "delete"
                             <void> ())
   window))

(define (delete-other-windows #!optional (window (selected-window)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "deleteOtherWindows"
                             <void> ())
   window))

(define (selected-window)
  ((primitive-static-method <gnu.jemacs.buffer.Window> "getSelected"
			    <gnu.jemacs.buffer.Window> ())))

(define (select-window window)
  ((primitive-static-method <gnu.jemacs.buffer.Window> "setSelected"
			    <void> (<gnu.jemacs.buffer.Window>))
   window)
  window)

;; Emacs allows extra options.
(define (next-window window)
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getNextWindowInFrame"
                             <gnu.jemacs.buffer.Window> (<int>))
   window 1))

;; Emacs allows some special values for frame.
(define (other-window #!optional (count 1) (frame (selected-frame)))
  (select-window
   ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getNextWindowInFrame"
                              <gnu.jemacs.buffer.Window> (<int>))
    (frame-selected-window frame) count)))

(define (window-buffer #!optional (window (selected-window)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getBuffer"
			    <buffer> ())
   window))

(define (switch-to-buffer
         #!optional (buffer (read-from-minibuffer "Switch to buffer: ")))
  (let ((buf (get-buffer buffer)))
    (if (eq? buf #!null)
        (set! buf (generate-new-buffer buffer)))
    (set-buffer buf)
    (set-window-buffer (selected-window) buf)))

(define (set-window-buffer window buffer)
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "setBuffer"
			    <void> (<buffer>))
   window (get-buffer buffer)))

(define (window-point window)
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getPoint" <int> ())
   window))

(define (set-window-point (window :: <window>) position)
  (invoke window 'setDot (invoke (invoke window 'getBuffer) 'positionToOffset position)))

;;; FRAMES

(define (make-frame #!optional (buffer (current-buffer)))
  ((primitive-constructor <gnu.jemacs.buffer.Frame> (<buffer>))
   buffer))

(define (delete-frame #!optional (frame (selected-frame)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Frame> "delete" <void> ())
   frame))

;; Emacs:  frame-live-p
(define (frame-live? frame)
  ((primitive-virtual-method <gnu.jemacs.buffer.Frame> "isLive" <boolean> ())
   frame))

(define (window-frame #!optional (window (selected-window)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getFrame"
                             <gnu.jemacs.buffer.Frame> ())
   window))

(define (frame-selected-window #!optional (frame (selected-frame)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Frame> "getSelectedWindow"
                             <gnu.jemacs.buffer.Window> ())
   frame))

(define (selected-frame)
  (invoke-static <gnu.jemacs.buffer.Frame> 'getSelectedFrame))

;;; POSITIONS

(define (point #!optional (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'getPoint))

(define (point-min #!optional (buffer :: <buffer> (current-buffer)))
  (+ (invoke buffer 'minDot) 1))

(define (point-max #!optional (buffer :: <buffer> (current-buffer)))
  (+ (invoke buffer 'maxDot) 1))

(define (buffer-end flag #!optional (buffer :: <buffer> (current-buffer)))
  ((if (<= flag 0) point-min point-max) buffer))

(define (buffer-size #!optional (buffer :: <buffer> (current-buffer)))
  (- (invoke buffer 'maxDot) (invoke buffer 'minDot)))

(define (goto-char position #!optional (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'setDot (invoke buffer 'positionToOffset position))
  (invoke buffer 'getDot))

(define (forward-char #!optional (count 1) (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "forwardChar" <void> (<int>))
   buffer count))

(define (backward-char #!optional (count 1) (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "backwardChar" <void> (<int>))
   buffer count))

(define (point-at-bol
         #!optional
         (n  :: <int> 1)
         (buffer  :: <buffer> (current-buffer)))
  <int>
  (let* ((orig (point buffer))
         (shortage (forward-line (- n 1) buffer))
         (end (point buffer)))
    (invoke buffer 'setPoint orig)
    end))

(define (point-at-eol #!optional (count  :: <int> 1)
                      (buffer  :: <buffer> (current-buffer)))
  <int>
  (let* ((pos-shortage
          (invoke buffer 'scan
                  #\Newline (invoke buffer 'getDot)
                  0 (- count (if (> count 0) 0 1)) #t))
         (shortage (arithmetic-shift pos-shortage -32))
         (pos (as <int> pos-shortage)))
    (if (zero? shortage) pos (+ pos 1))))

;; FIXME - ineffecient!
(define (goto-line line #!optional (buffer  :: <buffer> (current-buffer)))
  (goto-char (point-min buffer) buffer)
  (forward-line (- line 1) buffer))

(define (beginning-of-line
         #!optional
         (n  :: <int> 1)
         (buffer :: <buffer> (current-buffer))) <void>
  (invoke buffer 'setPoint (point-at-bol n buffer)))

(define (end-of-line #!optional
                     (n :: <int> 1)
                     (buffer :: <buffer> (current-buffer)))
  <void>
  (invoke buffer 'setPoint (point-at-eol n buffer)))

(define (forward-line #!optional
                      (count :: <int> 1)
                      (buffer :: <buffer> (current-buffer)))
  (let* ((content :: <gnu.jemacs.buffer.BufferContent>
                  (invoke buffer 'getContent))
         (negp (<= count 0))
         (pos2 (invoke buffer 'getDot))
         (pos-shortage
          (invoke buffer 'scan
                  #\Newline pos2 0
                  (- count (if negp 1 0)) #t))
         (shortage (arithmetic-shift pos-shortage -32))
         (pos (as <int> pos-shortage)))
    (if (and (> shortage 0)
             (or negp
                 (and (> (invoke buffer 'maxDot) (invoke buffer 'minDot))
                      (not (= pos pos2))
                      (not (char=? #\Newline
                                   (invoke content 'charAt (- pos 1)))))))
        (set! shortage (- shortage 1)))
    (invoke buffer 'setDot pos)
    (if negp (- shortage) shortage)))

(define (next-line #!optional (count 1) (buffer :: <buffer> (current-buffer)))
  (move-line count))

(define (previous-line #!optional (count 1) (buffer :: <buffer> (current-buffer)))
  (move-line (- count)))

(define (move-line arg #!optional (buffer ::  <buffer> (current-buffer)))
  (let ((goal-column (current-column buffer)))
    (forward-line arg buffer)
    (move-to-column goal-column #f buffer)))

;;; POSITIONS/EXCURSIONS

;;; MARKERS

(define-syntax save-excursion
  (syntax-rules ()
		((save-excursion form ...)
		 (let* ((save-buffer (current-buffer))
			(save-point (point save-buffer)))
		   (try-finally
		    (begin form ...)
		    (begin
		      (set-buffer save-buffer)
		      (goto-char save-point save-buffer)))))))

(define (make-marker)
  (make <gnu.jemacs.buffer.Marker>))

(define (point-marker #!optional (share #f) (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "getPointMarker"
                             <gnu.jemacs.buffer.Marker> (<boolean>))
   buffer share))


;; Emacs allows an integer as well as a marker.
(define (copy-marker (marker <gnu.jemacs.buffer.Marker>))
  ((primitive-constructor <gnu.jemacs.buffer.Marker> (<gnu.jemacs.buffer.Marker>))
   marker))

(define (marker-position (marker <gnu.jemacs.buffer.Marker>))
  (let ((value
         ((primitive-virtual-method <gnu.jemacs.buffer.Marker> "getPoint" <int> ())
          marker)))
    (if (= value 0) #f value)))

(define (marker-buffer (marker <gnu.jemacs.buffer.Marker>))
  ((primitive-virtual-method <gnu.jemacs.buffer.Marker> "getBuffer"
                             <buffer> ())
   marker))

(define (set-marker (marker <gnu.jemacs.buffer.Marker>) position
                    #!optional (buffer :: <buffer> (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Marker> "set" <void>
                             (<buffer> <int>))
   marker buffer (invoke buffer 'positionToOffset position)))

;;; TEXT

(define (insert-char ch #!optional (count 1) (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "insert" <void>
			     (<char> <int> <javax.swing.text.Style>))
   buffer ch count #!null))

(define (insert str
		#!optional (buffer :: <buffer> (current-buffer)))
  ;; FIXME char of str is a character
  (invoke buffer 'insert (invoke str 'toString) #!null))

(define (erase-buffer #!optional (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'removeAll))

(define (delete-region start end
		       #!optional (buffer  :: <buffer> (current-buffer)))
  (let ((start-offset :: <int>
		      (invoke buffer 'positionToOffset start))
	 (end-offset :: <int>
		     (invoke buffer 'positionToOffset end)))
  (invoke buffer 'removeRegion start-offset end-offset)))

(define (delete-char #!optional (count :: <int> 1) killp
		     (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'deleteChar count))

(define (delete-backward-char #!optional (count 1) killp
			      (buffer :: <buffer> (current-buffer)))
  (delete-char (- count) killp buffer))

;;; TEXT/COLUMNS

(define (current-column #!optional (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'currentColumn))

(define (move-to-column column
			#!optional force (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'moveToColumn column force))

;;; DEFAULT BINDINGS

(define (emacs-help)
  (format #t "Sorry - no help available.~%~!"))

(define-key global-map #(down) next-line)
(define-key global-map #(up) previous-line)
(define-key global-map #(backspace) delete-backward-char)
(define-key global-map #(left) backward-char)
(define-key global-map #(right) forward-char)
(define-key global-map "\C-a" beginning-of-line)
(define-key global-map "\C-b" backward-char)
(define-key global-map "\C-n" next-line)
(define-key global-map "\C-p" previous-line)
(define-key global-map "\C-d" delete-char)
(define-key global-map "\C-e" end-of-line)
(define-key global-map "\C-f" forward-char)
(define-key global-map "\C-x\C-s" save-buffer)
(define-key global-map "\C-x\C-w" write-file)
(define-key global-map "\C-x0" delete-window)
(define-key global-map "\C-x1" delete-other-windows)
(define-key global-map "\C-x2" split-window-vertically)
(define-key global-map "\C-x3" split-window-horizontally)
(define-key global-map "\C-xb" switch-to-buffer)
(define-key global-map "\C-xi" insert-file)
(define-key global-map "\C-x\C-f" find-file)
(define-key global-map "\C-x50" delete-frame)
(define-key global-map "\C-x52" make-frame)
(define-key global-map "\C-xo" other-window)
(define-key global-map '(control h) emacs-help)

(define (emacs)
  (set-buffer (get-buffer-create "*scratch*"))
  (make-frame))

;;; REPL

(define (term-send-input #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.ReplBuffer> "enter"
                             <void> ())
   buffer))

(define repl-map (make-keymap))
(define-key repl-map "\n" term-send-input)

(define (scheme-swing-window)
  (let ((buffer
         ((primitive-static-method <gnu.jemacs.buffer.ReplBuffer> "scheme"
                                   <gnu.jemacs.buffer.ReplBuffer> ()))))
    (use-local-map repl-map buffer)
    ; (make-frame buffer)
    (switch-to-buffer buffer)
    buffer))
