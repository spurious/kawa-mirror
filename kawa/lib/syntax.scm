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

(define (dynamic-wind before thunk after)
  (before)
  (try-finally
   (thunk)
   (after)))

(define-syntax fluid-let
  (syntax-rules ()
		((fluid-let ((variable init) ...) . exprs)
		 (let* ((old-env ((primitive-static-method
				   "kawa.lang.Environment" "getCurrent"
				   "kawa.lang.Environment" ())))
			(new-env ((primitive-constructor
				   "kawa.lang.Environment"
				   ("kawa.lang.Environment")) old-env)))
		   (begin
		     ((primitive-virtual-method "kawa.lang.Environment"
						"define" "kawa.lang.Binding"
						("String" <object>))
		      new-env 'variable init) ...)
		   ((primitive-static-method
		     "kawa.lang.Environment" "setCurrent"
		     "void" ("kawa.lang.Environment")) new-env)
		   (try-finally
		    (begin . exprs)
		    ((primitive-static-method
		      "kawa.lang.Environment" "setCurrent"
		      "void" ("kawa.lang.Environment")) old-env))))))
