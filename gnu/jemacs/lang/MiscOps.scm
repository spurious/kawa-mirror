;; FIXME
(define (buffer-disable-undo buf) '())

;; FIXME
(define (sit-for n) (sleep n))

;; FIXME
(define (input-pending-p) '())

;; FIXME
(define (message msg) (format #t msg))

;; FIXME
(define (provide name) '())

(define (purecopy obj) obj)

(define (call-interactively command)
  (invoke-static <gnu.jemacs.buffer.Command> 'perform command))

(define (minibuffer-depth) 0)  ;; FIXME

;; Should use stf, not setq, and guard against duplicate evaluation.  FIXME.
(define-syntax push
  (syntax-rules ()
		((push x place)
		 (setq place (cons x place)))))

(define (car x)
  (if (eq? x '()) x (slot-ref (as <pair> x) 'car)))

(define (cdr x)
  (if (eq? x '()) x (slot-ref (as <pair> x) 'cdr)))

(define (format fmt #!rest (args :: <Object[]>))
  (invoke-static 'kawa.standard.format 'formatToString #\% fmt args))

(define (apply func #!rest (args :: <Object[]>))
  (invoke-static 'kawa.standard.apply 'applyN
		 (if (symbol? func) (symbol-function func) func)
		 args))

(define-syntax prog1
  (syntax-rules ()
		((prog1 first)
		 first)
		((prog1 first rest ...)
		 (let ((%prog1-save% first)) ;; Should be lexical-let? FIXME
		   (begin rest ...)
		   %prog1-save%))))

(define (quit-char) #\bel)

