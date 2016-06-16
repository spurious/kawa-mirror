(module-name (kawa domterm))

(require <kawa.lib.prim_syntax>)

(define (domterm-load-stylesheet (styles::string) #!optional (name::string "Kawa")) ::void
  (kawa.DomTermBackend:loadStyleSheet name styles))
