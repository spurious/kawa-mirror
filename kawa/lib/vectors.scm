(define (vector? x)
  (instance? x <vector>))

(define (make-vector (k :: <int>) #!optional (fill #!undefined))
  (make <vector> k fill))

(define (vector #!rest (args :: <Object[]>))
  (make <vector> args))

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

(define (vector-fill! (vec :: <vector>) fill)
  (invoke vec 'setAll fill))
