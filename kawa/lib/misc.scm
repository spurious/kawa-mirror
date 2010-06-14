(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.strings>)
(require <kawa.lib.ports>)

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
  (instance? x <gnu.mapping.Symbol>))

(define (symbol->string (s <symbol>)) :: constant-string
  (s:toString))

(define-procedure symbol=?
  (lambda (s1::symbol s2::symbol)::boolean
	  (gnu.mapping.Symbol:equals s1 s2))
  (lambda (s1::symbol s2::symbol #!rest r)::boolean
	  (and (gnu.mapping.Symbol:equals s1 s2)
	       (apply symbol=? s2 r))))

(define (symbol-local-name s::symbol) ::constant-string
  (s:getLocalPart))

(define (symbol-namespace s::symbol) ::namespace
  (s:getNamespace))

(define (symbol-namespace-uri s::symbol) ::constant-string
  (s:getNamespaceURI))

(define (symbol-prefix s::symbol) ::constant-string
  (s:getPrefix))

(define (namespace-uri (ns::gnu.mapping.Namespace)) ::string
  (invoke ns 'getName))

(define (namespace-prefix (ns::gnu.mapping.Namespace)) ::string
  (invoke ns 'getPrefix))

(define (string->symbol (str <string>))
  (gnu.mapping.SimpleSymbol:valueOf (str:toString)))

(define (procedure? x) :: <boolean>
  (or (instance? x <function>) (instance? x gnu.kawa.lispexpr.LangObjType)))

(define (values #!rest (args :: <Object[]>))
  (invoke-static <gnu.mapping.Values> 'make args))


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

(define (scheme-implementation-version) :: constant-string
  (kawa.Version:getVersion))

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
  (kawa.lang.Promise:force arg))

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
		 (gnu.text.Path:currentPath)
		 ((as <gnu.kawa.xml.KNode> node):baseURI))))
    (if (eq? uri #!void) #f uri)))

#|
(define (identity-function x)
  x)

(define (make-parameter init #!optional converter :: <procedure> identity-function)

  (make <gnu.kawa.util.Parameter> init converter))
|#

(define (gentemp) :: <symbol>
  (invoke-static <gnu.expr.Symbols> 'gentemp))

(define (add-procedure-properties
	 (proc :: <gnu.expr.GenericProc>)
	 #!rest (args :: <object[]>)) :: <void>
  (invoke proc 'setProperties args))
