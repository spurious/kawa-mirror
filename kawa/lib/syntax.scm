(define-syntax defmacro
  (syntax-rules ()
		((defmacro name lambda-list form ...)
		 ((primitive-constructor <kawa.lang.DefMacro> 
					 (<symbol> <function>)) 
		  (quote name) (lambda lambda-list form ...)))))

(define (gentemp)
  ((primitive-static-method <gnu.expr.Symbol> "gentemp"
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

(define (catch key thunk handler)
  (try-catch (thunk)
	     (ex <kawa.lang.NamedException>
		 ((primitive-virtual-method
		   <kawa.lang.NamedException> "applyHandler"
		   <object> (<object> <function>))
		  ex key handler))))

;;; The one-argument case is a standard DSSSL procedure.
;;; The multi-argument extension matches Guile.
(define (error msg . args)
  (set! msg (call-with-output-string (lambda (port) (display msg port))))
  (set! args (map
	      (lambda (arg)
		(call-with-output-string (lambda (port) (write arg port))))
	      args))
  (apply throw 'misc-error msg args))

(define-syntax fluid-let
  (syntax-rules ()
		((fluid-let ((variable init) ...) . exprs)
		 (let* ((old-env ((primitive-static-method
				   "gnu.mapping.Environment" "getCurrent"
				   "gnu.mapping.Environment" ())))
			(new-env ((primitive-constructor
				   "gnu.mapping.Environment"
				   ("gnu.mapping.Environment")) old-env)))
		   (begin
		     ((primitive-virtual-method "gnu.mapping.Environment"
						"define" "gnu.mapping.Binding"
						("String" <object>))
		      new-env 'variable init) ...)
		   ((primitive-static-method
		     "gnu.mapping.Environment" "setCurrent"
		     "void" ("gnu.mapping.Environment")) new-env)
		   (try-finally
		    (begin . exprs)
		    ((primitive-static-method
		      "gnu.mapping.Environment" "setCurrent"
		      "void" ("gnu.mapping.Environment")) old-env))))))
