;; FUTURE

(define-syntax future (syntax-rules ()
				   ((delay expression)
				    (%make-future (lambda () expression)))))
