(define (vector-length x)
  ((primitive-virtual-method "kawa.lang.Vector" "length"
			     "int" ())
   x))

(define (vector-ref vector k)
  ((primitive-virtual-method "kawa.lang.Vector" "elementAt"
			     "Object" ("int"))
   vector k))

(define (vector-set! vector k obj)
  ((primitive-virtual-method "kawa.lang.Vector" "setElementAt"
			     "void" ("Object" "int"))
   vector obj k))

(define (list->vector x)
  ((primitive-virtual-method "kawa.lang.List" "toVector"
			     "kawa.lang.Vector" ())
   x))
