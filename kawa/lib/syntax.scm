(define-syntax defmacro
  (syntax-rules ()
		((defmacro name lambda-list form ...)
		 (%defmacro (quote name) (lambda lambda-list form ...)))))

(define (%defmacro name expander)
  ((primitive-constructor "kawa.lang.DefMacro"
			  (<symbol> <function>))
   name expander))

(define (gentemp)
  ((primitive-static-method "kawa.lang.Symbol" "gentemp"
			    <symbol> ())))

(define-syntax when (syntax-rules ()
				  ((when cond exp ...)
				   (if cond (begin exp ...)))))

(define-syntax unless (syntax-rules ()
				  ((when cond exp ...)
				   (if (not cond) (begin exp ...)))))
