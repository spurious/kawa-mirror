(define (vector? x)
  (instance? x <vector>))

(define (make-vector (k :: <int>) #!optional (fill #!undefined))
  (make <vector> k fill))

(define (vector #!rest (args :: <Object[]>))
  (make <vector> args))

(define (vector-length x :: <vector>)
  (invoke x 'size))

(define (vector-ref
         (vector <vector>)
         (k <int>))
  (invoke vector 'get k))

(define (vector-set!
         (vector <vector>)
         (k <int>)
         obj)
  (invoke vector 'set k obj))

(define (vector->list (vec :: <vector>)) :: <list>
  (let loop ((result :: <list> '())
	     (i :: <int> (vector-length vec)))
    (set! i (- i 1))
    (if (< i 0)
	result
	(loop (cons (vector-ref vec i) result) i))))

(define (list->vector (x <list>))
  ((primitive-constructor <vector> (<gnu.lists.Sequence>))
   x))

(define (vector-fill! (vec :: <vector>) fill)
  (invoke vec 'setAll fill))
