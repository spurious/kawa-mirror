;;; Helper macro for do, to handle optional step.
(define-syntax %do-step (syntax-rules ()
				      ((%do-step variable step) step)
				      ((%do-step variable) variable)))

(define-syntax do (syntax-rules ()
				((do ((variable init . step) ...)
				     (test sequence) commands ...)
				 (letrec ((loop
					   (lambda (variable ...)
					     (if test
						 (begin sequence)
						 (begin
						   commands ...
						   (loop (%do-step variable . step) ...))))))
				   (loop init ...))
				 )))
