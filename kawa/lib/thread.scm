(define (exit #!optional (status :: <int> 0))
  (invoke-static <output-port> 'runCleanups)
  (invoke-static <java.lang.System> 'exit statuc))

;; FUTURE

(define-syntax future (syntax-rules ()
				   ((future expression)
				    (%make-future (lambda () expression)))))
