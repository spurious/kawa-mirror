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
  ((primitive-virtual-method <String> "intern" <symbol> ())
   ((primitive-virtual-method <object> "toString" <String> ())
    str)))

(define (procedure? x)
  (instance? x <function>))

(define (environment-bound? env sym)
  (not (eq? ((primitive-virtual-method "gnu.mapping.Environment" "lookup"
                                       "gnu.mapping.Binding"
                                       (<symbol>))
             env sym)
       #!null)))

(define (scheme-implementation-version)
  (constant-fold
   (primitive-constructor <string> (<java.lang.String>))
   (constant-fold (primitive-static-method <kawa.Version> "getVersion"
					   <java.lang.String> ()))))
#|
(define (scheme-window #!optional share)
  ((primitive-constructor <kawa.GuiConsole> (<kawa.lang.Interpreter>))
   (if share
       ((primitive-constructor <kawa.standard.Scheme>(<gnu.mapping.Environment>))
	(interaction-environment))
       ((primitive-constructor <kawa.standard.Scheme> ())))))
|#
