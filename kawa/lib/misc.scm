#|
(define (equal? x y) :: <boolean>
  (or (eq? x y)
      (and (not (eq? x #!null))
	   ((primitive-virtual-method <object> "equals" <boolean> (<object>))
	    x y))))
|#

(define (boolean? x) :: <boolean>
  (or (eq? x #t) (eq? x #f)))

(define (symbol? x) :: <boolean>
  (or (instance? x <java.lang.String>)
      (instance? x <gnu.mapping.Symbol>)))

(define (symbol->string (s <symbol>))
  (make <string> s))

(define (string->symbol (str <string>))
  (invoke (invoke str 'toString) 'intern))

(define (procedure? x) :: <boolean>
  (instance? x <function>))

(define (values #!rest (args :: <Object[]>))
  (invoke-static <gnu.mapping.Values> 'make args))

(define-syntax (provide form)
  (syntax-case form ()
    ((provide 'feature)
     (cons (syntax define-constant)
	   (cons (datum->syntax-object
		  form
		  (string->symbol
		   (string-append "%provide%"
				  (symbol->string
				   (syntax-object->datum (syntax feature))))))
		 (syntax (:: <int> 123)))))
    ((_ . rest)
     (syntax-error form "provide requires a quoted feature-name"))))
	   

(define (environment-bound? (env :: <gnu.mapping.Environment>) sym)
  :: <boolean>
  (invoke env 'isBound
	  (gnu.kawa.lispexpr.LispLanguage:langSymbolToSymbol sym)))

;; The version number is not optional according to R5RS.
;; But since earlier versions of this implementation took 0 arguments,
;; we'll make it optional for backwards compatibility, at least for now.
(define (null-environment #!optional version)
  (static-field <kawa.standard.Scheme> 'nullEnvironment))

(define (scheme-report-environment version)
  (case version
    ((4) (static-field <kawa.standard.Scheme> 'r4Environment))
    ((5) (static-field <kawa.standard.Scheme> 'r5Environment))
    (else (error "scheme-report-environment version must be 4 or 5"))))

(define (interaction-environment)
  (invoke-static <gnu.mapping.Environment> 'user))

(define (scheme-implementation-version)
  (constant-fold
   (primitive-constructor <string> (<java.lang.String>))
   (constant-fold (primitive-static-method <kawa.Version> "getVersion"
					   <java.lang.String> ()))))
(define (set-procedure-property! proc :: <procedure> key value)
  (invoke proc 'setProperty key value))

(define-procedure procedure-property
  setter: set-procedure-property!
  (begin
    (define (procedure-property (proc :: <procedure>) key #!optional default)
      (invoke proc 'getProperty key default))
    procedure-property))

(define (dynamic-wind before thunk after)
  (before)
  (try-finally
   (thunk)
   (after)))

(define (force arg)
  (cond ((instance? arg <kawa.lang.Promise>)
	 (invoke (as <kawa.lang.Promise> arg) 'force))
	((instance? arg <gnu.mapping.Future>)
	 (invoke (as <gnu.mapping.Future> arg) 'waitForResult))
	(else arg)))

;;; The one-argument case is a standard DSSSL procedure.
;;; The multi-argument extension matches Guile.
(define (error msg . args)
  (set! msg (call-with-output-string (lambda (port) (display msg port))))
  (set! args (map
	      (lambda (arg)
		(call-with-output-string (lambda (port) (write arg port))))
	      args))
  (apply throw 'misc-error msg args))

(define (base-uri #!optional (node #!null))
  (let ((uri (if (eq? node #!null)
		 (invoke-static <gnu.kawa.functions.BaseUri> 'baseUri)
		 (invoke-static <gnu.kawa.functions.BaseUri> 'baseUri node))))
    (if (eq? uri #!void) #f uri)))

#|
(define (identity-function x)
  x)

(define (make-parameter init #!optional converter :: <procedure> identity-function)

  (make <gnu.kawa.util.Parameter> init converter))
|#
