(define-syntax delay (syntax-rules ()
				   ((delay expression)
				    (%make-promise (lambda () expression)))))
