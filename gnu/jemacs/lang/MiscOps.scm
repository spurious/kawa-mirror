;; FIXME
(define (buffer-disable-undo buf) '())

;; FIXME
(define (sit-for n) (sleep n))

;; FIXME
(define (input-pending-p) '())

;; FIXME
(define (message msg . args) (format #t msg))

;; FIXME
(define (provide name) '())

(define (purecopy obj) obj)

(define (call-interactively command)
  (invoke-static <gnu.jemacs.buffer.Command> 'perform command))

(define (minibuffer-depth) 0)  ;; FIXME

(define-syntax catch
  (syntax-rules ()
		((catch tag body ...)
		 (try-catch (begin body ...)
			    (ex <gnu.jemacs.lang.CatchableException>
				(invoke ex 'match tag))))))

(define (throw tag value) :: <never-returns>
  (primitive-throw
   (make <gnu.jemacs.lang.CatchableException> tag value)))

;; Should use setf, not setq, and guard against duplicate evaluation.  FIXME.
(define-syntax push
  (syntax-rules ()
		((push x place)
		 (setq place (cons x place)))))

(define (format fmt #!rest (args :: <Object[]>))
  (invoke-static 'gnu.kawa.functions.Format 'formatToString #\% fmt args))

(define (quit-char) #\bel)

(define (make-local-variable symbol)
  (invoke-static <gnu.jemacs.buffer.Buffer> 'makeBufferLocal symbol #f)
  symbol)
(define (make-variable-buffer-local symbol)
  (invoke-static <gnu.jemacs.buffer.Buffer> 'makeBufferLocal symbol #t)
  symbol)

(define (emacs:read #!optional (port (current-input-port)))
  (let ((lexer
	 ((primitive-constructor <gnu.jemacs.lang.ELispReader> (<input-port>))
	  port)))
    ((primitive-virtual-method <gnu.jemacs.lang.ELispReader> "readObject"
			       <object> ())
     lexer)))
