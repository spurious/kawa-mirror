(define (set-size win w h)
  ((primitive-virtual-method <java.awt.Component> "setSize" <void>
			     (<int> <int>))
   win w h))

;;; From here on, the functions are ordered as in the Lisp Reference
;;; Manual, XEmacs version 21.x.

;;; READ AND PRINT

(define (open-output-buffer buffer)
  ((primitive-constructor <gnu.jemacs.buffer.BufferWriter>
                          (<gnu.jemacs.buffer.Buffer>))
   buffer))

(define (open-output-marker marker)
  ((primitive-constructor <gnu.jemacs.buffer.BufferWriter>
                          (<gnu.jemacs.buffer.Marker>))
   marker))

;;; MINIBUFFERS

(define (read-dialog prompt)
  (symbol->string
   ((primitive-virtual-method <gnu.jemacs.buffer.Frame> "ask"
                              <java.lang.String> (<String>))
    (window-frame) prompt)))

(define read-from-minibuffer read-dialog)

;;; KEYMAPS

(define (make-keymap #!optional name)
  ((primitive-static-method <gnu.jemacs.buffer.BufferKeymap> "makeEmptyKeymap"
                            <javax.swing.text.Keymap> (<String>))
   name))

(define (keymap-name (keymap <javax.swing.text.Keymap>))
  ((primitive-virtual-method <javax.swing.text.Keymap> "getName" <java.lang.String> ())
   keymap))

(define global-map
  ((primitive-static-method <javax.swing.text.JTextComponent> "getKeymap"
			    <javax.swing.text.Keymap> (<java.lang.String>))
   ((primitive-get-static <javax.swing.text.JTextComponent> "DEFAULT_KEYMAP"
			  <java.lang.String>))))

(define (current-global-map)
  global-map)

(define (current-local-map #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.BufferKeymap> "getLocalKeymap"
                             <javax.swing.text.Keymap> ())
   ((primitive-get-field <gnu.jemacs.buffer.Buffer> "keymap"
                         <gnu.jemacs.buffer.BufferKeymap>)
    buffer)))

(define (use-local-map keymap #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.BufferKeymap> "setLocalKeymap"
                             <void> (<javax.swing.text.Keymap>))
   ((primitive-get-field <gnu.jemacs.buffer.Buffer> "keymap"
                         <gnu.jemacs.buffer.BufferKeymap>)
    buffer)
   keymap))

(define (lookup-key keymap keys #!optional accept-defaults)
  (let ((binding
         ((primitive-static-method <gnu.jemacs.buffer.BufferKeymap> "lookupKey"
                                   <object> (<javax.swing.text.Keymap>
                                             <kawa.lang.Sequence> <boolean>))
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
  ((primitive-static-method <gnu.jemacs.buffer.Buffer> "findFile"
                            <gnu.jemacs.buffer.Buffer> (<String>))
   filename))

(define (save-buffer #!optional (buffer (current-buffer)))
  (if (buffer-file-name buffer)
      ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "save" <void> ())
       (current-buffer))
      (write-file (read-from-minibuffer "File to save in: ") buffer)))

(define (write-file #!optional (filename (read-from-minibuffer "Write-file: "))
                    (buffer (current-buffer)))
  (set-visited-file-name filename buffer)
  (save-buffer buffer))

(define (insert-file filename #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "insertFile"
                             <void> (<String>))
   buffer filename))

;;; BUFFERS

(define (current-buffer)
  (invoke-static <gnu.jemacs.buffer.Buffer> 'getCurrent))

;; Emacs allows a buffer name as well as a buffer.
(define (set-buffer buffer)
  (invoke-static <gnu.jemacs.buffer.Buffer> 'setCurrent buffer))

;; Emacs returns an Emacs string, not a Java string. 
(define (buffer-name #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "getName"
			       <String> ())
   buffer))


(define (get-buffer buffer-or-name)
  ((primitive-static-method  <gnu.jemacs.buffer.Buffer> "coerceBuffer"
			     <gnu.jemacs.buffer.Buffer> (<object>))
   buffer-or-name))

(define (generate-new-buffer-name starting-name)
  ((primitive-static-method <gnu.jemacs.buffer.Buffer> "generateNewBufferName"
			    <String> (<String>))
   starting-name))

(define (buffer-file-name #!optional (buffer (current-buffer)))
  (let ((name
         ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "getFileName"
                                    <java.lang.String> ())
          buffer)))
    (if (eq? name #!null)
        #f
        (symbol->string name))))

(define (set-visited-file-name filename #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "setFileName" <void>
                             (<String>))
   buffer filename))

(define (get-buffer-create name)
  (let ((buf
	 ((primitive-static-method  <gnu.jemacs.buffer.Buffer> "getBuffer"
				    <gnu.jemacs.buffer.Buffer> (<String>))
	  name)))
    (if (eq? buf #!null)
	((primitive-constructor <gnu.jemacs.buffer.Buffer> (<String>)) name)
	buf)))

(define (generate-new-buffer name)
  ((primitive-constructor <gnu.jemacs.buffer.Buffer> (<String>))
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
			    <gnu.jemacs.buffer.Buffer> ())
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
			    <void> (<gnu.jemacs.buffer.Buffer>))
   window (get-buffer buffer)))

(define (window-point window)
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getPoint" <int> ())
   window))

(define (set-window-point window position)
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "setPoint" <void> (<int>))
   window position))

;;; FRAMES

(define (make-frame #!optional (buffer (current-buffer)))
  ((primitive-constructor <gnu.jemacs.buffer.Frame> (<gnu.jemacs.buffer.Buffer>))
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

(define (point #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "getPoint" <int> ())
   buffer))

(define (goto-char position #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "setPoint" <void> (<int>))
   buffer position))

(define (forward-char #!optional (count 1) (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "forwardChar" <void> (<int>))
   buffer count))

(define (backward-char #!optional (count 1) (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "backwardChar" <void> (<int>))
   buffer count))

;;; MARKERS

(define (make-marker)
  ((primitive-constructor <gnu.jemacs.text.Marker> ())))

(define (point-marker #!optional (share #f) (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "getPointMarker"
                             <gnu.jemacs.buffer.Marker> (<boolean>))
   buffer share))


;; Emacs allows an integer as well as a marker.
(define (copy-marker (marker <gnu.jemacs.text.Marker>))
  ((primitive-constructor <gnu.jemacs.text.Marker> (<gnu.jemacs.text.Marker>))
   marker))

(define (marker-position (marker <gnu.jemacs.text.Marker>))
  (let ((value
         ((primitive-virtual-method <gnu.jemacs.buffer.Marker> "getPoint" <int> ())
          marker)))
    (if (= value 0) #f value)))

(define (marker-buffer (marker <gnu.jemacs.text.Marker>))
  ((primitive-virtual-method <gnu.jemacs.buffer.Marker> "getBuffer"
                             <gnu.jemacs.buffer.Buffer> ())
   marker))

(define (set-marker (marker <gnu.jemacs.text.Marker>) position
                    #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Marker> "set" <void>
                             (<gnu.jemacs.buffer.Buffer> <int>))
   marker buffer (- position 1)))

;;; TEXT

(define (insert-char ch count #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "insert" <void>
			     (<char> <int> <javax.swing.text.Style>))
   buffer ch count #!null))

;;; DEFAULT BINDINGS

(define-key global-map "\C-b" backward-char)
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
(define-key global-map "\C-x52" make-frame)
(define-key global-map "\C-xo" other-window)
(define-key global-map "\C-x50" delete-frame)

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
