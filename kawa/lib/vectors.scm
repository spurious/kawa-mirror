(define (vector? x) :: <boolean>
  (instance? x <vector>))

(define (make-vector (k :: <int>) #!optional (fill #!undefined)) :: <vector>
  (make <vector> k fill))

(define (vector #!rest (args :: <Object[]>)) :: <vector>
  (make <vector> args))

(define (vector-length x :: <vector>) :: <int>
  (invoke x 'size))

(define (vector-set! (vector <vector>) (k <int>) obj) :: <void>
  (invoke vector 'set k obj))

(define-procedure vector-ref
  setter: vector-set!
  (begin
    (define (vector-ref (vector :: <vector>) (k :: <int>))
      (invoke vector 'get k))
    vector-ref))

(define (vector->list (vec :: <vector>)) :: <list>
  (let loop ((result :: <list> '())
	     (i :: <int> (vector-length vec)))
    (set! i (- i 1))
    (if (< i 0)
	result
	(loop (cons (vector-ref vec i) result) i))))

(define (list->vector (x :: <list>)) :: <vector>
  ((primitive-constructor <vector> (<gnu.lists.Sequence>))
   x))

(define (vector-fill! (vec :: <vector>) fill) :: <void>
  (invoke vec 'setAll fill))
