#|
(define (equal? x y)
  (or (eq? x y)
      (and (not (eq? x #!null))
	   ((primitive-virtual-method <object> "equals" <boolean> (<object>))
	    x y))))
|#

(define (boolean? x)
  (instance? x <java.lang.Boolean>))

(define (symbol? x)
  (instance? x <java.lang.String>))

(define (symbol->string (s <symbol>))
  (make <string> s))

(define (string->symbol (str <string>))
  (invoke (invoke str 'toString) 'intern))

(define (procedure? x)
  (and (instance? x <function>) (not (instance? x <gnu.mapping.Location>))))

(define (values #!rest (args :: <Object[]>))
  (invoke-static <gnu.mapping.Values> 'make args))

(define (environment-bound? env sym)
  (not (eq? ((primitive-virtual-method "gnu.mapping.Environment" "lookup"
                                       "gnu.mapping.Binding"
                                       (<symbol>))
             env sym)
       #!null)))

(define (null-environment)
  (static-field <kawa.standard.Scheme> 'nullEnvironment))

(define (interaction-environment)
  (invoke-static <gnu.mapping.Environment> 'user))

(define (scheme-implementation-version)
  (constant-fold
   (primitive-constructor <string> (<java.lang.String>))
   (constant-fold (primitive-static-method <kawa.Version> "getVersion"
					   <java.lang.String> ()))))
(define (procedure-property (proc :: <procedure>) key #!optional default)
  (invoke proc 'getProperty key default))

(define (set-procedure-property! proc :: <procedure> key value)
  (invoke proc 'setProperty key value))

;;; The one-argument case is a standard DSSSL procedure.
;;; The multi-argument extension matches Guile.
(define (error msg . args)
  (set! msg (call-with-output-string (lambda (port) (display msg port))))
  (set! args (map
	      (lambda (arg)
		(call-with-output-string (lambda (port) (write arg port))))
	      args))
  (apply throw 'misc-error msg args))
