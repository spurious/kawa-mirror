(define-syntax defmacro
  (syntax-rules ()
		((defmacro name pattern form ...)
                 (define-syntax name
                   (lambda (__arg)
                     (syntax-case __arg ()
                                  ((__name . pattern) (begin form ...))))))))

(define (%defmacro form rule)
  (rule (car (form 'form))))

;; A future version may allow (this [<CLASS>])
(define-syntax this
  (lambda (__arg)
    (make <gnu.expr.ThisExp>)))

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
