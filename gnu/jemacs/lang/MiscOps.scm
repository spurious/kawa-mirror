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

(define (format fmt #!rest (args :: <Object[]>))
  (invoke-static 'kawa.standard.format 'formatToString #\% fmt args))

(define (quit-char) #\bel)

