(define-syntax %do-trace
  (syntax-rules ()
		((%do-trace proc flag)
		 (set! proc
		       ((primitive-static-method
			 <kawa.standard.TracedProcedure> "doTrace"
			 <kawa.standard.TracedProcedure>
			 (<function> <boolean>))
			proc flag)))))

(define-syntax trace
  (syntax-rules ()
		((trace proc ...)
		 (begin (%do-trace proc #t) ...))))

(define-syntax untrace
  (syntax-rules ()
		((untrace proc ...)
		 (begin (%do-trace proc #f) ...))))

