(define (vector? x)
  (instance? x <vector>))

(define (vector-length x :: <vector>)
  (invoke x 'length))

(define (vector-ref
         (vector <vector>)
         (k <int>))
  (invoke vector 'elementAt k))

(define (vector-set!
         (vector <vector>)
         (k <int>)
         obj)
  (invoke vector 'setElementAt obj k))

(define (list->vector (x <list>))
  (invoke x 'toVector))
