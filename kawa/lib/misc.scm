(define (environment-bound? env sym)
  (not (eq? ((primitive-virtual-method "kawa.lang.Environment" "lookup"
                                       "kawa.lang.Binding"
                                       (<symbol>))
             env sym)
       #!null)))

(define (scheme-implementation-version)
  (constant-fold
   (primitive-constructor <string> (<java.lang.String>))
   (constant-fold (primitive-static-method <kawa.Version> "getVersion"
					   <java.lang.String> ()))))

(define (scheme-window #!optional share)
  ((primitive-constructor <kawa.GuiConsole> (<kawa.lang.Interpreter>))
   (if share
       ((primitive-constructor <kawa.standard.Scheme>(<kawa.lang.Environment>))
	(interaction-environment))
       ((primitive-constructor <kawa.standard.Scheme> ())))))
