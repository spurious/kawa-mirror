(define-syntax defmacro
  (syntax-rules ()
		((defmacro name lambda-list form ...)
		 (%defmacro name (lambda lambda-list form ...)))))

(define (%defmacro name expander)
  ((primitive-constructor "kawa.lang.DefMacro"
			  ("String" "kawa.lang.Procedure"))
   name expander))

(define (gentemp)
  ((primitive-static-method "kawa.lang.Symbol" "gentemp"
			    "kawa.lang.Symbol" ())))

(define-syntax when (syntax-rules ()
				  ((when cond exp ...)
				   (if cond (begin exp ...)))))

(define-syntax unless (syntax-rules ()
				  ((when cond exp ...)
				   (if (not cond) (begin exp ...)))))
