(define (environment-bound? env sym)
  (not (eq? ((primitive-virtual-method "kawa.lang.Environment" "lookup"
                                       "kawa.lang.Binding"
                                       (<symbol>))
             env sym)
       #!null)))

;;; Here %V is a macro defined in the Makefile.
(define (scheme-implementation-version) (%V))

(define (scheme-window #!optional share)
  ((primitive-constructor <kawa.GuiConsole> (<kawa.lang.Interpreter>))
   (if share
       ((primitive-constructor <kawa.standard.Scheme>(<kawa.lang.Environment>))
	(interaction-environment))
       ((primitive-constructor <kawa.standard.Scheme> ())))))
