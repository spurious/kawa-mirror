;; FIXME should return <elisp:boolean>, not <boolean>
(define (arrayp x)
  (instance? x <gnu.kawa.util.UniformVector>))

(define (aref
         (array <gnu.kawa.util.UniformVector>)
         (k <int>))
  (invoke array 'elementAt k))

(define (aset (array <gnu.kawa.util.UniformVector>)
	      (k <int>)
	      obj)
  (invoke array 'setElementAt obj k)
  obj)

(define (fillarray (array <gnu.kawa.util.UniformVector>) obj)
  (invoke array 'setAll obj)
  obj)
