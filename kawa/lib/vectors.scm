(define (vector-length x)
  ((primitive-virtual-method <vector> "length" <int> ())
   x))

(define (vector-ref vector k)
  ((primitive-virtual-method <vector> "elementAt"
			     <object> (<int>))
   vector k))

(define (vector-set! vector k obj)
  ((primitive-virtual-method <vector> "setElementAt"
			     <void> (<object> <int>))
   vector obj k))

(define (list->vector x)
  ((primitive-virtual-method <list> "toVector" <vector> ())
   x))
