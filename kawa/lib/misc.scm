(define (environment-bound? env sym)
  (not (eq? ((primitive-virtual-method "kawa.lang.Environment" "lookup"
                                       "kawa.lang.Binding"
                                       ("kawa.lang.Symbol"))
             env sym)
       #!null)))

