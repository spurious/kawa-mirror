(define (length (x :: <gnu.lists.Sequence>))
  (invoke x 'size))

(define (arrayp x) <elisp:boolean>
  (instance? x <gnu.lists.SimpleVector>))

(define (aref
         (array <gnu.lists.SimpleVector>)
         (k <int>))
  (invoke array 'get k))

(define (aset (array <gnu.lists.SimpleVector>)
	      (k <int>)
	      obj)
  (invoke array 'set k obj)
  obj)

(define (fillarray (array <gnu.lists.SimpleVector>) obj)
  (invoke array 'fill obj)
  obj)
