(define (exit #!optional (status 0))
  ((primitive-static-method "java.lang.System" "exit"
			    "void" ("int"))
   status))  

;; FUTURE

(define-syntax future (syntax-rules ()
				   ((delay expression)
				    (%make-future (lambda () expression)))))
