(define (set-size win w h)
  ((primitive-virtual-method <java.awt.Component> "setSize" <void>
			     (<int> <int>))
   win w h))

;;; From here on, the functions are ordered as in the Lisp Reference
;;; Manual, XEmacs version 21.x.

;;; KEYMAPS

(define global-map
  ((primitive-static-method <javax.swing.text.JTextComponent> "getKeymap"
			    <javax.swing.text.Keymap> (<java.lang.String>))
   ((primitive-get-static <javax.swing.text.JTextComponent> "DEFAULT_KEYMAP"
			  <java.lang.String>))))

(define (current-global-map)
  global-map)

(define (define-key keymap key binding)
  ((primitive-static-method <gnu.jemacs.buffer.Keymap> "defineKey" <void>
			    (<javax.swing.text.Keymap> <object> <object>))
   keymap key binding))

;;; BUFFERS

(define *current-buffer* #f)

(define (current-buffer) *current-buffer*)

;; Emacs allows a buffer name as well as a buffer.
(define (set-buffer buffer) (set! *current-buffer* buffer))

;; Emacs returns an Emacs string, not a Java string. 
(define (buffer-name #!optional (buffer (current-buffer)))
  ((primitive-virtual-function <gnu.jemacs.buffer.Buffer> "getName"
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

(define (split-window-vertically #!optional arg)
  ; "Split current window into two windows, one above the other."
  ; (interactive "P")
  (split-window (selected-window) arg #f))

(define (split-window-horizontally #!optional arg)
  ; "Split current window into two windows, one above the other."
  ; (interactive "P")
  (split-window (selected-window) arg #t))

(define (selected-window)
  ((primitive-static-method <gnu.jemacs.buffer.Window> "getSelected"
			    <gnu.jemacs.buffer.Window> ())))

(define (select-window window)
  ((primitive-static-method <gnu.jemacs.buffer.Window> "setSelected"
			    <void> (<gnu.jemacs.buffer.Window>))
   window))

(define (window-buffer #!optional (window (selected-window)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getBuffer"
			    <gnu.jemacs.buffer.Buffer> ())
   window))

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

(define (make-frame)
  ((primitive-constructor <gnu.jemacs.buffer.Frame> (<gnu.jemacs.buffer.Buffer>))
   (current-buffer)))

(define (window-frame #!optional (window (selected-window)))
  ((primitive-get-field <gnu.jemacs.buffer.Window> "frame" <gnu.jemacs.buffer.Frame>)
   window))

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

;;; TEXT

(define (insert-char ch count #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Buffer> "insert" <void>
			     (<char> <int>))
   buffer ch count))

;;; DEFAULT BINDINGS

(define-key global-map (string (integer->char 2)) backward-char)
(define-key global-map (string (integer->char 6)) forward-char)

(define (emacs)
  (set-buffer (get-buffer-create "*scratch*"))
  (make-frame))
