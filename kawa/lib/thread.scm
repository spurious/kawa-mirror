(define (exit #!optional (status :: <int> 0))
  (invoke-static <output-port> 'runCleanups)
  (invoke-static <java.lang.System> 'exit status))

;; FUTURE

(define-syntax future (syntax-rules ()
				   ((future expression)
				    (%make-future (lambda () expression)))))

(define-private (%make-future (p :: <procedure>)) :: <gnu.mapping.Future>
  (let ((f :: <gnu.mapping.Future> (make <gnu.mapping.Future> p)))
    (invoke f 'start)
    f))
