(define (vector? x)
  (instance? x <vector>))

(define (vector-length (x <vector>))
  ((primitive-virtual-method <vector> "length" <int> ())
   x))

(define (vector-ref (vector <vector>) (k <int>))
  ((primitive-virtual-method <vector> "elementAt"
			     <object> (<int>))
   vector k))

(define (vector-set! (vector <vector>) (k <int>) obj)
  ((primitive-virtual-method <vector> "setElementAt"
			     <void> (<object> <int>))
   vector obj k))

(define (list->vector (x <list>))
  ((primitive-virtual-method <list> "toVector" <vector> ())
   x))
